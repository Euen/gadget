-module(gadget_plain_dtl_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {dtl :: atom()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, [Module]) ->
  {ok, Req, #state{dtl = Module}}.

-spec handle(cowboy_req:req(), #state{}) -> ok.
handle(Req, State = #state{dtl = Module}) ->
  Headers = [{<<"content-type">>, <<"text/html">>}],
  {ok, Body} = Module:render([]),
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) -> ok.
