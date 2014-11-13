-module(gadget_callback_handler).

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
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    case cowboy_req:qs_val(<<"code">>, Req) of
        {undefined, Req2} ->
            Body = [<<"Missing 'code' querystring parameter.">>],
            {ok, Req3} = cowboy_req:reply(400, Headers, Body, Req2),
            {ok, Req3, State};
        {Code, Req2} ->
            case access_token(Code) of
                {ok, Token} ->
                    Url = "/repos",
                    RedirHeaders = [{"Location", Url}],
                    Req3 = cowboy_req:set_resp_cookie(
                                   <<"token">>, Token, [{path, <<"/">>}], Req2
                                  ),
                    {ok, Req4} = cowboy_req:reply(302, RedirHeaders, Req3),
                    {ok, Req4, State};
                {error, Reason} ->
                    lager:info("Error: ~p", [Reason]),
                    Body = [<<"Error: ">>, Reason],
                    {ok, Req3} = cowboy_req:reply(400, Headers, Body, Req2),
                    {ok, Req3, State}
            end
    end.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access_token(Code) ->
    {ok, ClientId} = application:get_env(gadget, github_client_id),
    {ok, ClientSecret} = application:get_env(gadget, github_client_secret),
    lager:info("~p - ~p", [ClientId, ClientSecret]),
    Url = "https://github.com/login/oauth/access_token",
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"},
               {"Accept", "application/json"}],
    Body = ["code=", Code,
            "&client_id=", ClientId,
            "&client_secret=", ClientSecret],
    Opts = [{ssl_options, [{depth, 0}]}],
    case ibrowse:send_req(Url, Headers, post, Body, Opts) of
        {ok, "200", _RespHeaders, RespBody} ->
            JsonBody = jiffy:decode(RespBody, [return_maps]),
            case maps:is_key(<<"access_token">>, JsonBody) of
                true ->
                    Token = maps:get(<<"access_token">>, JsonBody),
                    {ok, Token};
                false ->
                    {error, RespBody}
            end;
        {ok, Status, _, _} ->
            {error, Status};
        {error, Reason} ->
            {error, Reason}
    end.
