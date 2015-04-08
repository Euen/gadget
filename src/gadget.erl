-module(gadget).
-behavior(application).

-export([ start/0
        , start/2
        , stop/1
        , start_phase/3
        , webhook/2
        ]).
-export([ register/3
        , unregister/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application Behavior
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(application:start_type(), term()) ->
  {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
  gadget_sup:start_link().

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  application:stop(mnesia),
  mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(cowboy, http_port, 8585),
  ListenerCount = application:get_env(cowboy, http_listener_count, 10),
  Dispatch = cowboy_router:compile([
    {'_', [ %% Web UI
            {"/", gadget_plain_dtl_handler, [index_dtl]}
          , {"/repos", gadget_repos_handler, []}
          , {"/active-tools", gadget_on_handler, []}
          , {"/active-tools/:tool", gadget_off_handler, []}
          , {"/assets/[...]", cowboy_static, {dir, "assets"}}
            %% OAuth
          , {"/login", gadget_login_handler, []}
          , {"/callback", gadget_callback_handler, []}
            %% Webhook
          , {"/webhook/:tool", gadget_webhook_handler, []}
          ]
    }
  ]),
  RanchOpts = [{port, Port}],
  CowboyOpts =
    [ {env,       [{dispatch, Dispatch}]}
    , {compress,  true}
    , {timeout,   12000}
    ],
  case cowboy:start_http(http_gadget, ListenerCount, RanchOpts, CowboyOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

-spec stop(term()) -> ok.
stop(_State) ->
  cowboy:stop_listener(http_gadget),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> ok.
start() ->
  application:ensure_all_started(gadget).

-spec webhook(binary(), map()) -> ok | {error, term()}.
webhook(ToolName, RequestMap) ->
  #{ mod := Mod
   , tool := Tool
   , name := Name
   , context := Context
   } = gadget_utils:webhook_info(ToolName),

  Repo = get_repo_name(RequestMap),
  Cred = github_credentials(),

  case gadget_repos_repo:fetch(Repo, Tool) of
    notfound ->
      egithub_webhook:event(Mod, Cred, RequestMap);
    GadgetRepo ->
      Token = gadget_repos:token(GadgetRepo),
      StatusCred = egithub:oauth(Token),
      egithub_webhook:event(Mod, StatusCred, Name, Context, Cred, RequestMap)
  end.

-spec register(string(), atom(), string()) -> gadget_repos:repo().
register(Repo, Tool, Token) -> gadget_repos_repo:register(Repo, Tool, Token).

-spec unregister(string(), atom()) -> 0|1.
unregister(Repo, Tool) -> gadget_repos_repo:unregister(Repo, Tool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Not exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec github_credentials() -> egithub:credentials().
github_credentials() ->
  User = application:get_env(gadget, github_user, ""),
  Password = application:get_env(gadget, github_password, ""),
  egithub:basic_auth(User, Password).

-spec get_repo_name(map()) -> string().
get_repo_name(#{body := Body}) ->
  EventData = jiffy:decode(Body, [return_maps]),
  #{<<"repository">> := Repository} = EventData,
  maps:get(<<"full_name">>, Repository, <<>>).
