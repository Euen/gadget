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
      Result = egithub:create_webhook(Cred, Repo, WebhookUrl, ["pull_request"]),
    case check_result(Result) of
      {ok, _} ->
        gadget_repos_repo:register(Repo, Tool, Token),
        io:format("Webhook added!~n"),
        gadget_repos_repo:register(Repo, Tool, Token),
        true;
      {error, {"422", _, _}} ->
        io:format("Webhook already exists.~n"),
        false
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

-spec check_result(term()) -> term().
check_result({error, {"422", _, _}}) ->
  throw(webhook_already_exists);
check_result({error, {"401", _, _}}) ->
  throw(unauthorized);
check_result({error, {"404", _, _}}) ->
  throw(not_found);
check_result(Result) ->
  Result.
