%%% @doc GET /webhook/:tool handler
-module(gadget_webhook_handler).

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
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  {ToolName, Req1} = cowboy_req:binding(tool, Req),
  {Headers, Req2} = cowboy_req:headers(Req1),
  HeadersMap = maps:from_list(Headers),
  {ok, Body, Req3} = cowboy_req:body(Req2),
  RequestMap = #{headers => HeadersMap,
                 body => Body},
  % Run checks just for these pull request's actions
  DefaultPRActions = [<<"opened">>, <<"reopened">>, <<"synchronize">>],
  Actions = application:get_env(gadget, pr_actions, DefaultPRActions),
  BodyJson = jiffy:decode(Body, [return_maps]),
  Action = maps:get(<<"action">>, BodyJson, <<"">>),
  % Checks action first to avoid failing when first payload is sent by GitHub
  % just after gadget register the webhook in the repository.
  % This is because the first payload does not contain `action` key.
  case lists:member(Action, Actions) of
    true ->
      #{<<"pull_request">> :=
        #{<<"head">> :=
          #{<<"repo">> :=
            #{<<"owner">> :=
              #{<<"login">> := Org}}}}} = BodyJson,
      % Only process requests for valid organizations repositories
      ValidOrgs = application:get_env(gadget, valid_organizations, []),
      case lists:member(Org, ValidOrgs) of
        true -> process_request(ToolName, RequestMap, Req3, State);
        false -> return(403, <<"Event not  processed.">>, Req3, State)
      end;
    false -> return(200, <<"Event ignored.">>, Req3, State)
  end.

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.

%% internal
-spec process_request(binary(), map(), cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), state()}.
process_request(ToolName, RequestMap, Req3, State) ->
  case gadget:webhook(ToolName, RequestMap) of
    {error, Reason} ->
      RespBody = [<<"There was an error while processing the event: ">>,
                  io_lib:format("~p", [Reason])],
      return(400, RespBody, Req3, State);
    _Result ->
      return(200, <<"Event processed.">>, Req3, State)
  end.

return(Status, RespBody, Req, State) ->
  RespHeaders = [{<<"content-type">>, <<"text/plain">>}],
  {ok, Req1} = cowboy_req:reply(Status, RespHeaders, RespBody, Req),
  {ok, Req1, State}.
