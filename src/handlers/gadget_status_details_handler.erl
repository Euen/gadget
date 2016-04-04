%%% @doc Status handler
-module(gadget_status_details_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok | shutdown, cowboy_req:req(), term()}.
init(_Type, Req, _) ->
  {ok, Req, []}.

%% @private
-spec handle(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
handle(Req, State) ->
  Headers = [{<<"content-type">>, <<"text/html">>}],
  {Id, Req1} = cowboy_req:binding(id, Req),

  Log = gadget_logs_repo:fetch(binary_to_integer(Id)),
  Repo = gadget_logs:repository(Log),
  PrNumber = gadget_logs:pr_number(Log),
  Log1 = gadget_logs:back_url(Log, back_url(Repo, PrNumber)),
  {ok, Body} = status_dtl:render(Log1),
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req1),
  {ok, Req2, State}.

%% @private
-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) -> ok.

-spec back_url(string(), integer()) -> string().
back_url(Repo, PrNumber) ->
  lists:flatten([ "https://github.com/"
                , Repo
                , "/pull/"
                , integer_to_list(PrNumber)
                ]).
