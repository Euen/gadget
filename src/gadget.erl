-module(gadget).
-behavior(application).

-export([ start/0
        , start/2
        , stop/1
        , webhook/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application Behavior
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    {ok, Pid} = gadget_sup:start_link(),
    start_cowboy_listeners(),
    {ok, Pid}.

-spec stop(term()) -> ok.
stop(_State) ->
    cowboy:stop_listener(http_gadget),
    ok.

%% Shell start function

-spec start() -> ok.
start() ->
    application:ensure_all_started(gadget).

start_cowboy_listeners() ->
  {ok, Port} = application:get_env(cowboy, http_port),
  {ok, ListenerCount} = application:get_env(cowboy, http_listener_count),
  Dispatch = cowboy_router:compile([
    {'_', [
            %% Web UI
            {"/", gadget_plain_dtl_handler, [index_dtl]},
            {"/repos", gadget_repos_handler, []},
            {"/active-tools", gadget_on_handler, []},
            {"/active-tools/:tool", gadget_off_handler, []},
            {"/assets/[...]", cowboy_static, {dir, "assets"}},
            %% OAuth
            {"/login", gadget_login_handler, []},
            {"/callback", gadget_callback_handler, []},
            %% Webhook
            {"/webhook/:tool", gadget_webhook_handler, []}
          ]
    }
  ]),
  RanchOpts =
      [
        {port, Port}
      ],
  CowboyOpts =
      [
       {env,       [{dispatch, Dispatch}]},
       {compress,  true},
       {timeout,   12000}
      ],
  cowboy:start_http(http_gadget, ListenerCount, RanchOpts, CowboyOpts).

-spec webhook(atom(), map()) -> ok | {error, term()}.
webhook(elvis, RequestMap) -> elvis:webhook(RequestMap);
webhook(compiler, RequestMap) -> gadget_compiler:webhook(RequestMap).