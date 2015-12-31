%%% @doc GET /webhook/:tool handler
-module(gadget_webhook_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec handle(cowboy_req:req(), #state{}) -> ok.
handle(Req, State) ->
  {ToolName, Req1} = cowboy_req:binding(tool, Req),
  {Headers, Req2} = cowboy_req:headers(Req1),
  HeadersMap = maps:from_list(Headers),
  {ok, Body, Req3} = cowboy_req:body(Req2),
  RequestMap = #{headers => HeadersMap,
                 body => Body},
  % Run checks just for these pull request's actions
  Actions = [<<"opened">>, <<"reopened">>, <<"synchronize">>],
  BodyJson = jiffy:decode(Body, [return_maps]),
  Action = maps:get(<<"action">>, BodyJson, <<"">>),
  case lists:member(Action, Actions) of
    true -> process_request(ToolName, RequestMap, Req3, State);
    false -> {ok, Req3, State}
  end.

%% @private
-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) -> ok.

%% internal
process_request(ToolName, RequestMap, Req3, State) ->
  case gadget:webhook(ToolName, RequestMap) of
    {error, Reason} ->
      Status = 400,
      RespHeaders = [{<<"content-type">>, <<"text/plain">>}],
      RespBody = [<<"There was an error while processing the event: ">>,
                  io_lib:format("~p", [Reason])],
      {ok, Req4} = cowboy_req:reply(Status, RespHeaders, RespBody, Req3),
      {ok, Req4, State};
    _Result ->
      Status = 200,
      RespHeaders = [{<<"content-type">>, <<"text/plain">>}],
      RespBody = <<"Event processed.">>,
      {ok, Req4} = cowboy_req:reply(Status, RespHeaders, RespBody, Req3),
      {ok, Req4, State}
  end.
