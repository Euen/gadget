-module(gadget_server).
-behavior(application).

-export([
         start/0,
         start/2,
         stop/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application Behavior
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    gadget_server_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    cowboy:stop_listener(http_gadget_server),
    ok.

%% Shell start function

-spec start() -> ok.
start() ->
    application:ensure_all_started(gadget_server).
