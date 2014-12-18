-module(gadget_elvis).

-export([
         on/4
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec on(term(), term(), term(), term()) -> ok.
on(Repo, Cred, Events, ToolName) ->
  {ok, RepoInfo} = egithub:repo(Cred, Repo),
  ok =
    case RepoInfo of
      #{<<"private">> := true} ->
        add_user(Cred, RepoInfo, get_username(ToolName));
      _ ->
        ok
   end,

  WebhookUrl = elvis_utils:to_str(get_url(ToolName)),
  {ok, _Hook} = egithub:create_webhook(Cred, Repo, WebhookUrl, Events).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_user(Cred, RepoInfo, Username) ->
  #{ <<"full_name">> := Repo
   , <<"owner">> := Owner
   } = RepoInfo,
  #{ <<"type">> := Type
   , <<"login">> := Login} = Owner,

  case Type of
    <<"User">> -> egithub:add_collaborator(Cred, Repo, Username);
    _Org -> add_user_to_org(Cred, Login, Repo, Username)
  end.

add_user_to_org(Cred, Org, Repo, Username) ->
  {ok, _} = egithub:create_team(Cred, Org, "Services", "pull", [Repo]),
  {ok, Teams} = egithub:teams(Cred, Org),
  [TeamId] = [TeamId
              || #{<<"id">> := TeamId, <<"name">> := Name} <- Teams,
                 <<"Services">> == Name],
  ok = egithub:add_team_repository(Cred, TeamId, Repo),
  egithub:add_team_member(Cred, TeamId, Username).

get_username(ToolName) ->
  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  Tool = maps:get(ToolName, WebhookMap),
  maps:get(username, Tool).

get_url(ToolName) ->
  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  maps:get(ToolName, WebhookMap).
