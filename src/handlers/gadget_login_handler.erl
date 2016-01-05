%%% @doc GET /login handler
-module(gadget_login_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok | shutdown, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec handle(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  {ok, ClientId} = application:get_env(gadget, github_client_id),
  {ok, Scope} = application:get_env(gadget, github_scope),
  Url = "https://github.com/login/oauth/authorize?"
      ++ "client_id=" ++ ClientId
      ++ "&scope=" ++ Scope,
  Headers = [{<<"Location">>, Url}],
  Body = [],
  {ok, Req2} = cowboy_req:reply(302, Headers, Body, Req),
  {ok, Req2, State}.

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.
