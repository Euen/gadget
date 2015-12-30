%%% @doc Main application module
-module(gadget).
-behavior(application).

-export([ start/0
        , start/2
        , stop/1
        , start_phase/3
        , webhook/2
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application Behavior
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec start(application:start_type(), term()) ->
  {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
  gadget_sup:start_link().

%% @private
-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(cxy_ctl_setup, _StartType, []) ->
  case cxy_ctl:init([{webhook, unlimited, 1000, 100000}]) of
    true -> ok;
    {error, Error} -> {error, Error}
  end;
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  ok = mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(cowboy, http_port, 8585),
  ListenerCount = application:get_env(cowboy, http_listener_count, 10),
  Dispatch = cowboy_router:compile([
    {'_', [ %% Web UI
            {"/", gadget_plain_dtl_handler, [index_dtl]}
          , {"/favicon.ico", cowboy_static, {file, "assets/favicon.ico"}}
          , {"/config", gadget_plain_dtl_handler, [config_dtl]}
          , {"/about", gadget_plain_dtl_handler, [about_dtl]}
          , {"/repos", gadget_repos_handler, []}
          , {"/active-tools", gadget_on_handler, []}
          , {"/active-tools/:tool", gadget_off_handler, []}
          , {"/assets/[...]", cowboy_static, {dir, "assets"}}
            %% OAuth
          , {"/login", gadget_login_handler, []}
          , {"/callback", gadget_callback_handler, []}
            %% Webhook
          , {"/webhook/:tool", gadget_webhook_handler, []}
            %% Status
          , {"/status", gadget_status_handler, []}
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

%% @private
-spec stop(term()) -> ok.
stop(_State) ->
  cowboy:stop_listener(http_gadget).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc starts the application
-spec start() -> {ok, [atom()]}.
start() ->
  application:ensure_all_started(gadget).

%% @doc runs a particular webhook on a PR
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
      Args = [Mod, StatusCred, Name, Context, Cred, RequestMap],
      cxy_ctl:execute_task(webhook, egithub_webhook, event, Args)
  end.

-spec get_repo_name(map()) -> string().
get_repo_name(#{body := Body}) ->
  EventData = jiffy:decode(Body, [return_maps]),
  #{<<"repository">> := Repository} = EventData,
  maps:get(<<"full_name">>, Repository, <<>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Not exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec github_credentials() -> egithub:credentials().
github_credentials() ->
  User = application:get_env(gadget, github_user, ""),
  Password = application:get_env(gadget, github_password, ""),
  egithub:basic_auth(User, Password).

