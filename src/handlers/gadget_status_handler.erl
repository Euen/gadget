%%% @doc GET /status handler
-module(gadget_status_handler).
-author('elbrujohalcon@inaka.net').

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
  {ok, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  Headers = [{<<"content-type">>, <<"text/plain">>}],
  Body = "Server is Up",
  {ok, Req1} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req1, State}.

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.
