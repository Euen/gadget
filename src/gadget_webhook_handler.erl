-module(gadget_webhook_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

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
    ToolName = cowboy_req:binding(tool, Req),
    {Headers, Req} = cowboy_req:headers(Req),
    HeadersMap = maps:from_list(Headers),
    {ok, Body, Req2} = cowboy_req:body(Req),
    RequestMap = #{headers => HeadersMap,
                   body => Body},

    lager:info("~p", [Headers]),
    case gadget:webhook(ToolName, RequestMap) of
        {error, Reason} ->
            Status = 400,
            RespHeaders = [{<<"content-type">>, <<"text/plain">>}],
            RespBody = [<<"There was an error while processing the event: ">>,
                        gadget_utils:to_str(Reason)],
            {ok, Req3} = cowboy_req:reply(Status, RespHeaders, RespBody, Req2),
            {ok, Req3, State};
        _Result ->
            Status = 200,
            RespHeaders = [{<<"content-type">>, <<"text/plain">>}],
            RespBody = <<"Event processed.">>,
            {ok, Req3} = cowboy_req:reply(Status, RespHeaders, RespBody, Req2),
            {ok, Req3, State}
    end.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
