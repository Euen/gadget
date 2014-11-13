-module(gadget_on_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

-include("gadget_server.hrl").

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
    {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
    {ok, Req, #state{}}.

-spec handle(cowboy_req:req(), #state{}) -> ok.
handle(Req, State) ->
    {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
    {Repo, _} = cowboy_req:qs_val(<<"repo">>, Req, ""),
    Cred = egithub:oauth(Token),

    {ok, RepoInfo} = egithub:repo(Cred, Repo),
    ok = case RepoInfo of
             #{<<"private">> := true} ->
                 add_user(Cred, RepoInfo, ?ELVISINAKA);
             _ ->
                 ok
         end,

    Events = ["pull_request"],
    WebhookUrl = gadget_utils:to_str(?WEBHOOK_URL),
    {ok, _Hook} = egithub:create_webhook(Cred, Repo, WebhookUrl, Events),

    Headers = [{<<"content-type">>, <<"text/html">>}],
    Body = [],
    {ok, Req2} = cowboy_req:reply(204, Headers, Body, Req),
    {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_user(Cred, RepoInfo, Username) ->
    #{<<"full_name">> := Repo,
      <<"owner">> := Owner} = RepoInfo,
    #{<<"type">> := Type,
      <<"login">> := Login} = Owner,

    case Type of
        <<"User">> ->
            egithub:add_collaborator(Cred, Repo, ?ELVISINAKA);
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
