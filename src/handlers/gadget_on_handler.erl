%%% @doc POST /on handler
-module(gadget_on_handler).

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_accepted/2
        , handle_post/2
        , terminate/3
        ]).

-record(state, {}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @private
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

%% @private
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), atom()}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

%% @private
-spec handle_post(cowboy_req:req(), state()) ->
  {true | false, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  Decoded = jiffy:decode(Body, [return_maps]),
  Tool = binary_to_atom(maps:get(<<"tool">>, Decoded, <<"">>), utf8),
  {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
  Repo = maps:get(<<"repo">>, Decoded, ""),
  Result = gadget_core:register(Repo, Tool, Token),
  {Result, Req1, State}.

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.
