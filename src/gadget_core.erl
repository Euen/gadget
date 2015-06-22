%%% @doc gadget_core module
-module(gadget_core).

-export([register/3]).
-export([unregister/3]).
-export([repositories/1]).
-export([repo_info/2]).

%% @doc registers a repo for processing
-spec register(string(), atom(), string()) -> boolean().
register(Repo, Tool, Token) ->
  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  case maps:get(Tool, WebhookMap, false) of
    false -> false;
    WebhookUrl ->
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

-spec repositories(egithub:credentials()) -> list().
repositories(Cred) ->
  Opts = #{type => <<"owner">>},
  {ok, Repos} = egithub:all_repos(Cred, Opts),
  {ok, Orgs} = egithub:orgs(Cred),

  OrgReposFun =
    fun(#{<<"login">> := OrgName}) ->
      {ok, OrgRepos} = egithub:all_org_repos(Cred, OrgName, Opts),
      OrgRepos
    end,
  AllOrgsRepos = lists:flatmap(OrgReposFun, Orgs),

  PublicRepos =
    [Repo || Repo <- Repos ++ AllOrgsRepos,
             gadget_utils:is_admin(Repo)],

  lists:sort([repo_info(Cred, Repo) || Repo <- PublicRepos]).

-spec repo_info(egithub:credentials(), map()) -> [tuple()].
repo_info(Cred, Repo) ->
  Name = maps:get(<<"name">>, Repo),
  FullName = maps:get(<<"full_name">>, Repo),
  HtmlUrl = maps:get(<<"html_url">>, Repo),
  Hooks =
    case egithub:hooks(Cred, FullName) of
      {ok, HooksResult} -> HooksResult;
      {error, _} -> []
    end,

  Status = gadget_utils:active_tools(Hooks),
  StatusList = lists:map(fun maps:to_list/1, Status),
  [ {full_name, FullName}
  , {name, Name}
  , {html_url, HtmlUrl}
  , {status, StatusList}
  ].

-spec check_result(term()) -> term().
check_result({error, {"422", _, _}}) ->
  throw(webhook_already_exists);
check_result({error, {"401", _, _}}) ->
  throw(unauthorized);
check_result({error, {"404", _, _}}) ->
  throw(not_found);
check_result(Result) ->
  Result.

