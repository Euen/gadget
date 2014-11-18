-module(gadget_elvis).

-export([
         on/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec on(term(), term(), term()) -> ok.
on(Repo, Cred, Events) ->
  {ok, RepoInfo} = egithub:repo(Cred, Repo),
  
  ok = case RepoInfo of
           #{<<"private">> := true} ->
               add_user(Cred, RepoInfo, get_username());
           _ ->
               ok
       end,

  WebhookUrl = elvis_utils:to_str(get_url()),
  {ok, _Hook} = egithub:create_webhook(Cred, Repo, WebhookUrl, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_user(Cred, RepoInfo, Username) ->
    #{<<"full_name">> := Repo,
      <<"owner">> := Owner} = RepoInfo,
    #{<<"type">> := Type,
      <<"login">> := Login} = Owner,

    WebhookMap = application:get_env(gadget, webhooks),
    ElvisWebhook = maps:get("elvis", WebhookMap),
    WhkUsername = maps:get("username", ElvisWebhook),
    case Type of
        <<"User">> ->
            egithub:add_collaborator(Cred, Repo, WhkUsername);
        _Org ->
            add_user_to_org(Cred, Login, Repo, Username)
    end.

add_user_to_org(Cred, Org, Repo, Username) ->
    {ok, _} = egithub:create_team(Cred, Org, "Services", "pull", [Repo]),
    {ok, Teams} = egithub:teams(Cred, Org),
    [TeamId] = [TeamId
                || #{<<"id">> := TeamId, <<"name">> := Name} <- Teams,
                   <<"Services">> == Name],
    ok = egithub:add_team_repository(Cred, TeamId, Repo),
    egithub:add_team_member(Cred, TeamId, Username).

get_username() ->
  WebhookMap = application:get_env(gadget, webhooks),
  ElvisWebhook = maps:get("elvis", WebhookMap),
  maps:get("username", ElvisWebhook).

get_url() ->
  WebhookMap = application:get_env(gadget, webhooks),
  ElvisWebhook = maps:get("elvis", WebhookMap),
  maps:get("url", ElvisWebhook).