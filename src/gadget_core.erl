%%% @doc Main application module
-module(gadget_core).

-export([ register/3
        , unregister/3
        , github_credentials/0 %%QUITAR!!!!!!!!!!
        ]).

%% @doc registers a repo for processing
-spec register(string(), atom(), string()) -> gadget_repos:repo().
register(Repo, Tool, Token) ->
  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  case maps:get(Tool, WebhookMap, false) of
    false -> false;
    WebhookUrl ->
      Cred = egithub:oauth(Token),
      {ok, _Hook} =
        egithub:create_webhook(Cred, Repo, WebhookUrl, ["pull_request"]),
      gadget_repos_repo:register(Repo, Tool, Token),
      true
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Not exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec github_credentials() -> egithub:credentials().
github_credentials() ->
  User = application:get_env(gadget, github_user, ""),
  Password = application:get_env(gadget, github_password, ""),
  egithub:basic_auth(User, Password).

-spec get_repo_name(map()) -> string().
get_repo_name(#{body := Body}) ->
  EventData = jiffy:decode(Body, [return_maps]),
  #{<<"repository">> := Repository} = EventData,
  maps:get(<<"full_name">>, Repository, <<>>).
