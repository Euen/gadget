-module(gadget_off_handler).

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

    ok = remove_user(Cred, Repo, ?ELVISINAKA),
    {ok, Hooks} = egithub:hooks(Cred, Repo),
    case gadget_server_utils:hook_by_url(?WEBHOOK_URL, Hooks) of
        {ok, Hook} ->
            #{<<"id">> := Id} = Hook,
            ok = egithub:delete_webhook(Cred, Repo, Id);
        not_found ->
            not_found
    end,

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

%% @doc Only remove the user if the repo belong to a user, otherwise the
%%      the user should not be removed from the Services team just in case
%%      it is being used in another project from the same organisation.
remove_user(Cred, Repo, Username) ->
    {ok, RepoInfo} = egithub:repo(Cred, Repo),
    Owner = maps:get(<<"owner">>, RepoInfo),
    #{<<"type">> := Type} = Owner,

    case Type of
        <<"User">> ->
            egithub:remove_collaborator(Cred, Repo, Username);
        _Org ->
            ok
    end.
