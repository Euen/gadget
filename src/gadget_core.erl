%%% @doc gadget_core module
-module(gadget_core).

-export([register/3,
         unregister/3,
         sync_repositories/2,
         filter_repositories/2,
         repo_info/2]).

%% @doc registers a repo for processing
-spec register(string(), atom(), string()) -> boolean().
register(Repo, Tool, Token) ->
  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  case maps:get(Tool, WebhookMap, false) of
    false -> false;
    #{url := WebhookUrl} ->
      Cred = egithub:oauth(Token),
      try
        Result =
          egithub:create_webhook(Cred, Repo, WebhookUrl, ["pull_request"]),
        case check_result(Result) of
          {ok, webhook_already_exists} -> % already exists
            RepoInfo = gadget_repos_repo:fetch_by_full_name(Repo),
            % This hook already exists for this repository, so fetch the
            % updated list of hooks for this repo (avoid the user to sync
            % all the repositories hooks).
            _ = sync_repo_hooks(Cred, RepoInfo),
            ok;
          {ok, Hook} ->
            % Hook created on github for this repo. Save info locally
            % (hooks cache purposes).
            #{<<"id">> := Id, <<"config">> := #{<<"url">> := Url}} = Hook,
            RepoInfo = gadget_repos_repo:fetch_by_full_name(Repo),
            RepoId = gadget_repos:id(RepoInfo),
            _ = gadget_repo_hooks_repo:register(Id, RepoId, Url),
            ok
        end,
        _ = gadget_repo_tools_repo:register(Repo, Tool, Token),
        true
      catch
        _ -> false
      end
  end.

%% @doc unregisters a repo
-spec unregister(binary(), atom(), string()) -> 0|1.
unregister(Repo, Tool, Token) ->
  Cred = egithub:oauth(Token),
  RepoInfo = gadget_repos_repo:fetch_by_full_name(Repo),
  RepoId = gadget_repos:id(RepoInfo),
  Hooks = gadget_repo_hooks_repo:fetch_by_repo_id(RepoId),
  EnabledTools = gadget_utils:active_tools(Hooks),
  HIds =
    [HookId
     || #{ hook_id := HookId
         , name    := ToolName
         , status  := on
         } <- EnabledTools
     , ToolName == Tool
     ],
  case HIds of
    [] -> ok;
    [Id] ->
      Msg = "IF THERE IS A 404 ERROR BELOW THIS MESSAGE, IGNORE IT, IT IS "
            "BECAUSE GADGET TRIED TO DELETE A NON-EXISTING WEBHOOK. issue#292",
      _ = lager:info(Msg, []),
      case egithub:delete_webhook(Cred, Repo, Id) of
        ok -> ok;
        % For the times when the webhook has been deleted manually using
        % the repo settings
        {error, {404, _, _}} -> ok
      end,
      _ = gadget_repo_hooks_repo:unregister(Id),
      ok
  end,
  gadget_repo_tools_repo:unregister(Repo, Tool).

-spec sync_repositories(Cred::egithub:credentials(),
                        User::gadget_users:user()) -> [gadget_repos:repo()].
sync_repositories(Cred, User) ->
  Opts = #{type => <<"owner">>, per_page => 100},
  {ok, Repos} = egithub:all_repos(Cred, Opts),
  {ok, Orgs} = egithub:orgs(Cred),

  OrgsOpts = Opts#{type => <<"public">>},
  OrgReposFun =
    fun(#{<<"login">> := OrgName}) ->
      {ok, OrgRepos} = egithub:all_org_repos(Cred, OrgName, OrgsOpts),
      OrgRepos
    end,
  AllOrgsRepos = lists:flatmap(OrgReposFun, Orgs),

  % Get all the public repositories the user is an admin of.
  % This avoids getting repos we can’t manage anyway.
  AllRepos =
    [Repo ||
     #{<<"private">> := false,
       <<"permissions">> := #{<<"admin">> := true}} = Repo <-
     Repos ++ AllOrgsRepos],

  % Store the repos in mnesia and associate them with the given user
  SavedRepos = [register_repo(Repo#{<<"languages">> => []}) ||
                Repo <- AllRepos], % Cache repos info
  % Associate repo_ids to the user for future requests
  RepoIds = [gadget_repos:id(Repo) || Repo <- SavedRepos],
  ok = associate_repo_ids_to_user(RepoIds, User),

  _ = [sync_repo_languages_and_hooks(Cred, Repo) || Repo <- SavedRepos],
  SavedRepos.

-spec filter_repositories(Filter::binary(),
                          Repos::[gadget_repos:repo()]) ->
  [gadget_repos:repo()].
filter_repositories(<<"all">>, Repos) ->
  get_all_repos(Repos);
filter_repositories(_Filter, Repos) ->
  get_supported_repos(Repos).

-spec repo_info(Repo::gadget_repos:repo(), Filter::binary()) -> [tuple()].
repo_info(Repo, Filter) ->
  Name = gadget_repos:name(Repo),
  FullName = gadget_repos:full_name(Repo),
  HtmlUrl = gadget_repos:html_url(Repo),
  {ok, Tools} = application:get_env(gadget, webhooks),
  RepoLangsSupported = gadget_repos:languages(Repo),
  Hooks = gadget_repo_hooks_repo:fetch_by_repo_id(gadget_repos:id(Repo)),

  Status =
    lists:map(
      fun(#{name := ToolName} = State) ->
        Tool = maps:get(ToolName, Tools),
        ToolLangs = maps:get(languages, Tool),
        case {RepoLangsSupported, Filter} of
          {_, <<"all">>} ->
            % Don't disable "unsupported" tools.
            State;
          {[], _} ->
            % repo.language = null -> enable all the tools
            State;
          {_, _} ->
            case RepoLangsSupported -- ToolLangs of
              RepoLangsSupported ->
                % Current tool does not support any of the languages
                % used in this repo. User is disallowed to activate it.
                State#{status => disable};
              _ ->
                % Current tool supports at least one of the languages
                % used in this repo. User is allowed to activate it.
                State
            end
        end
      end,
      gadget_utils:active_tools(Hooks)
     ),

  StatusList = lists:map(fun maps:to_list/1, Status),
  [ {full_name, FullName}
  , {name, Name}
  , {html_url, HtmlUrl}
  , {status, StatusList}
  ].

%% =============================================================================
%% Private
%% =============================================================================

-spec check_result(term()) -> term().
check_result({error, {422, _, _}}) ->
  {ok, webhook_already_exists};
check_result({error, {"401", _, _}}) ->
  throw(unauthorized);
check_result({error, {"404", _, _}}) ->
  throw(not_found);
check_result(Result) ->
  Result.

-spec register_repo(Repo::map()) ->
  gadget_repos:repo().
register_repo(Repo) ->
  #{<<"id">> := Id,
    <<"name">> := Name,
    <<"full_name">> := FullName,
    <<"html_url">> := HtmlUrl,
    <<"private">> := Private,
    <<"permissions">> := #{<<"admin">> := Admin,
                           <<"pull">> := Pull,
                           <<"push">> := Push},
    <<"languages_url">> := LangsUrl,
    <<"language">> := Lang,
    <<"languages">> := Langs} = Repo,
gadget_repos_repo:register(
  Id, Name, FullName, HtmlUrl, Private, Admin, Pull, Push, LangsUrl, Lang, Langs
  ).

%% @private
-spec associate_repo_ids_to_user(RepoIds::[gadget_users:repo_ids()],
                                 User::gadget_users:user()) ->
  ok.
associate_repo_ids_to_user(RepoIds, User) ->
  UserId = gadget_users:id(User),
  UserName = gadget_users:username(User),
  _ = gadget_users_repo:register(UserId, UserName, RepoIds),
  ok.

-spec sync_repo_languages_and_hooks(Cred::egithub:credentials(),
                                    Repo::gadget_repos:repo()) ->
  ok.
sync_repo_languages_and_hooks(Cred, Repo) ->
  ok = sync_repo_languages(Cred, Repo),
  _ = sync_repo_hooks(Cred, Repo),
  ok.

-spec sync_repo_languages(Cred::egithub:credentials(),
                          Repo::gadget_repos:repo()) ->
  ok.
sync_repo_languages(Cred, Repo) ->
  RepoId = gadget_repos:id(Repo),
  FullName = gadget_repos:full_name(Repo),
  {ok, LanguagesMap} = egithub:languages(Cred, binary_to_list(FullName)),
  Languages = maps:keys(LanguagesMap),
  ok = gadget_repos_repo:update_languages(RepoId, Languages).

-spec sync_repo_hooks(Cred::egithub:credentials(), Repo::gadget_repos:repo()) ->
  [gadget_repo_hooks:repo_hook()].
sync_repo_hooks(Cred, Repo) ->
  RepoId = gadget_repos:id(Repo),
  FullName = gadget_repos:full_name(Repo),
  % Remove all the existing hooks for the given repo_id
  _ =
    lists:foreach(
      fun(RepoHook) ->
        gadget_repo_hooks_repo:unregister(gadget_repo_hooks:id(RepoHook))
      end,
      gadget_repo_hooks_repo:fetch_by_repo_id(RepoId)
    ),
  case egithub:hooks(Cred, FullName) of
    {ok, HooksResult} ->
      % Store and return the list of stored Hooks for the given repo
      [gadget_repo_hooks_repo:register(Id, RepoId, Url) ||
       #{<<"id">> := Id, <<"config">> := #{<<"url">> := Url}} <-
       HooksResult];
    {error, _} ->
      []
  end.

-spec get_all_repos(Repos::[gadget_repos:repo()]) ->
  [gadget_repos:repo()].
get_all_repos(Repos) ->
  [Repo || Repo <- Repos,
   gadget_utils:is_admin(Repo),
   gadget_utils:is_public(Repo)].

-spec get_supported_repos(Repos::[gadget_repos:repo()]) ->
  [gadget_repos:repo()].
get_supported_repos(Repos) ->
  lists:filtermap(
    fun(Repo) ->
      gadget_utils:is_admin(Repo) andalso
      gadget_utils:is_public(Repo) andalso
      gadget_utils:is_supported(Repo)
    end,
    Repos).
