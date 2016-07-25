%%% @doc gadget_core module
-module(gadget_core).

-export([register/3]).
-export([unregister/3]).
-export([repositories/2]).
-export([repo_info/2]).

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
        {ok, _} = check_result(Result),
        _ = gadget_repos_repo:register(Repo, Tool, Token),
        true
      catch
        _ -> false
      end
  end.

%% @doc unregisters a repo
-spec unregister(string(), atom(), string()) -> 0|1.
unregister(Repo, Tool, Token) ->
  Cred = egithub:oauth(Token),
  {ok, Hooks} = egithub:hooks(Cred, Repo),
  EnabledTools = gadget_utils:active_tools(Hooks),
  HIds =
    [HookId
     || #{ hook_id := HookId
         , name    := ToolName
         , status  := on
         } <- EnabledTools
     , ToolName == Tool
     ],
  ok =
    case HIds of
      [] -> ok;
      [Id] -> egithub:delete_webhook(Cred, Repo, Id)
    end,
  gadget_repos_repo:unregister(Repo, Tool).

-spec repositories(egithub:credentials(), binary()) -> list().
repositories(Cred, Filter) ->
  Opts = #{type => <<"owner">>, per_page => 100},
  {ok, UserOrgsList} = egithub:orgs(Cred),
  UserOrgs = lists:map(fun(#{<<"login">> := OrgName}) -> OrgName end,
                       UserOrgsList),
  ValidOrgs = application:get_env(gadget, valid_organizations, []),
  % Only show repos for valid organizations
  Orgs = lists:filter(
    fun(Org) -> lists:member(Org, ValidOrgs) end,
    UserOrgs
  ),
  OrgReposFun =
    fun(OrgName) ->
      {ok, OrgRepos} = egithub:all_org_repos(Cred, OrgName, Opts),
      OrgRepos
    end,
  AllOrgsRepos = lists:flatmap(OrgReposFun, Orgs),

  case Filter of
    <<"all">> ->
      get_all_repos(Cred, AllOrgsRepos);
    _ ->
      get_supported_repos(Cred, AllOrgsRepos)
  end.

-spec repo_info(egithub:credentials(), map()) -> [tuple()].
repo_info(Cred, Repo) ->
  Name = maps:get(<<"name">>, Repo),
  FullName = maps:get(<<"full_name">>, Repo),
  HtmlUrl = maps:get(<<"html_url">>, Repo),
  {ok, Tools} = application:get_env(gadget, webhooks),
  RepoLangsSupported = maps:get(<<"languages">>, Repo),
  Hooks =
    case egithub:hooks(Cred, FullName) of
      {ok, HooksResult} -> HooksResult;
      {error, _} -> []
    end,

  Status =
    lists:map(
      fun(#{name := ToolName} = State) ->
        Tool = maps:get(ToolName, Tools),
        ToolLangs = maps:get(languages, Tool),
        case RepoLangsSupported of
          [] -> % repo.language = null -> enable all the tools
            State;
          _ ->
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
check_result({error, {"422", _, _}}) ->
  throw(webhook_already_exists);
check_result({error, {"401", _, _}}) ->
  throw(unauthorized);
check_result({error, {"404", _, _}}) ->
  throw(not_found);
check_result(Result) ->
  Result.

-spec get_all_repos(Cred::egithub:credentials(), Repos::[map()]) ->
  list().
get_all_repos(Cred, Repos) ->
  AllRepos =
    [Repo || Repo <- Repos,
     gadget_utils:is_admin(Repo),
     (not gadget_utils:is_public(Repo))],

  lists:sort([
    repo_info(Cred, Repo#{<<"languages">> => []}) || Repo <- AllRepos]).

-spec get_supported_repos(Cred::egithub:credentials(), Repos::[map()]) ->
  list().
get_supported_repos(Cred, Repos) ->
  SupportedRepos =
    lists:filtermap(
      fun(Repo) ->
        gadget_utils:is_admin(Repo) andalso
        (not gadget_utils:is_public(Repo)) andalso
        gadget_utils:is_supported(Repo, Cred)
      end,
      Repos),

  lists:sort([repo_info(Cred, Repo) || Repo <- SupportedRepos]).
