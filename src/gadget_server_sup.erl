-module(gadget_server_sup).
-behavior(supervisor).

-export([
         start_link/0,
         start_listeners/0
        ]).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, {}).

-spec start_listeners() -> {ok, pid()}.
start_listeners() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    {ok, ListenerCount} = application:get_env(cowboy, http_listener_count),

    Dispatch =
        cowboy_router:compile(
          [ {'_',
             [
              %% Web UI
              {"/", gadget_plain_dtl_handler, [index_dtl]},
              {"/repos", gadget_repos_handler, []},
              {"/on", gadget_on_handler, []},
              {"/off", gadget_off_handler, []},
              {"/assets/[...]", cowboy_static, {dir, "assets"}},
              %% OAuth
              {"/login", gadget_login_handler, []},
              {"/callback", gadget_callback_handler, []},
              %% Webhook
              {"/webhook", gadget_webhook_handler, []}
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

    cowboy:start_http(http_gadget_server, ListenerCount, RanchOpts, CowboyOpts).

%%% Supervisor Behavior

-spec init(term()) -> term().
init({}) ->
    {
      ok,
      {
        {one_for_one, 5, 10},
        [
         {http_gadget_server,
          {?MODULE, start_listeners, []}, permanent, 1000, worker, [?MODULE]}
        ]
      }}.
