%%% @doc Main application module
-module(gadget_core).

-export([ register/3
        , unregister/3
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
