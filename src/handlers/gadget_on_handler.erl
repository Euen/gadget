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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @private
-spec rest_init(cowboy_req:req(), term()) ->
  {ok, cowboy_req:req(), #state{}}.
rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec allowed_methods(cowboy_req:req(), term()) ->
  {[], cowboy_req:req(), term()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

%% @private
-spec content_types_accepted(cowboy_req:req(), term()) ->
  {[], cowboy_req:req(), #state{}}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

%% @private
-spec handle_post(cowboy_req:req(), #state{}) -> ok.
handle_post(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  Decoded = jiffy:decode(Body, [return_maps]),
  Tool = binary_to_atom(maps:get(<<"tool">>, Decoded, <<"">>), utf8),

  {ok, WebhookMap} = application:get_env(gadget, webhooks),
  case maps:get(Tool, WebhookMap, false) of
    false ->
      {false, Req1, State};
    WebhookUrl ->
      Repo = maps:get(<<"repo">>, Decoded),
      {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
      Cred = egithub:oauth(Token),
      {ok, _Hook} =
        egithub:create_webhook(Cred, Repo, WebhookUrl, ["pull_request"]),
      gadget:register(Repo, Tool, Token),
      {true, Req1, State}
  end.

%% @private
-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) -> ok.
