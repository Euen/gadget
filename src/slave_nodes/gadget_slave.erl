-module(gadget_slave).
-behaviour(gen_server).

-export([ start_link/1
        , start/1
        , stop/1
        ]).

-export([ init/1
        , terminate/2
        , code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-type state() :: #{ slave_port => port()
                  , slave_node => atom()
                  , wait_for_eol => boolean()
                  }.

-spec start(atom()) -> {ok, pid()}.
start(NodeName) -> gadget_slave_sup:start_child(NodeName).

-spec start_link(atom()) -> {ok, pid()}.
start_link(NodeName) ->
  gen_server:start_link({local, NodeName}, ?MODULE, NodeName, []).

-spec stop(pid() | atom()) -> ok.
stop(PidOrNodeName) -> gen_server:cast(PidOrNodeName, stop).

%% @private
-spec init(atom()) -> {ok, state()} | {stop, term()}.
init(NodeName) ->
  Erl = application:get_env(gadget, erl_path, os:find_executable("erl")),
  SlaveNode = atom_to_list(NodeName),
  ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
  Ping = iolist_to_binary(io_lib:format("net_adm:ping(~p)", [node()])),
  Port =
    erlang:open_port(
      {spawn_executable, Erl},
      [ {line, 2048}
      , stderr_to_stdout
      , {args,
        [ "-name" , SlaveNode
        , "-setcookie", erlang:get_cookie()
        , "-noshell"
        , "-boot", "start_clean"
        , "-eval", Ping
        ]}
      ]),
  wait_for_node(
    #{slave_port => Port, slave_node => NodeName, wait_for_eol => false}).

%% @private
-spec handle_call(Msg, _From, state()) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, state()}.
handle_call(Msg, _From, State) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, State}.

%% @private
-spec handle_info({nodedown, atom()}, state()) ->
  {stop, nodedown, state()} | {noreply, state()}.
handle_info({nodedown, SlaveNode, Info}, State = #{slave_node := SlaveNode}) ->
  lager:error("Slave node ~p is down! (~p)", [SlaveNode, Info]),
  {stop, nodedown, State};
handle_info({Port, {data, {eol, SlaveLog}}},
            State = #{slave_port := Port, wait_for_eol := false}) ->
  #{slave_node := SlaveNode} = State,
  HR = lists:duplicate(80, $~),
  lager:debug("~n~s~n(~p)> ~s~n~s", [HR, SlaveNode, SlaveLog, HR]),
  {noreply, State};
handle_info({Port, {data, {eol, SlaveLog}}},
            State = #{slave_port := Port, wait_for_eol := true}) ->
  #{slave_node := SlaveNode} = State,
  HR = lists:duplicate(80, $~),
  lager:debug("~n(~p)> ~s~n~s", [SlaveNode, SlaveLog, HR]),
  {noreply, State};
handle_info({Port, {data, {noeol, SlaveLog}}}, State = #{slave_port := Port}) ->
  #{slave_node := SlaveNode} = State,
  HR = lists:duplicate(80, $~),
  lager:debug("~n~s~n(~p)> ~s", [HR, SlaveNode, SlaveLog]),
  {noreply, State};
handle_info(Info, State) ->
  #{slave_node := SlaveNode} = State,
  HR = lists:duplicate(80, $~),
  lager:warning("~n~s~n(~p)> ~p", [HR, SlaveNode, Info]),
  {noreply, State}.

%% @private
-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) ->
  lager:debug("Stopping ~p", [State]),
  {stop, normal, State}.

%% @private
-spec terminate(_, state()) -> true.
terminate(_Reason, State) ->
  #{slave_port := SlavePort, slave_node := SlaveNode} = State,
  erlang:port_close(SlavePort),
  rpc:cast(SlaveNode, init, stop, []).

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


wait_for_node(State = #{slave_node := SlaveNode}) ->
  receive
    {nodeup, SlaveNode, Info} ->
      lager:debug("Node ~p is up! (~p)", [SlaveNode, Info]),
      {ok, State};
    Info ->
      case handle_info(Info, State) of
        {noreply, NewState} ->
          wait_for_node(NewState);
        {stop, Reason, _NewState} ->
          {stop, Reason}
      end
  after 5000 ->
    lager:debug("Node ~p never went up :(", [SlaveNode]),
    {stop, nodedown}
  end.
