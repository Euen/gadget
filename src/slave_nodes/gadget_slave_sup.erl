-module(gadget_slave_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_child(atom()) -> {ok, pid()}.
start_child(NodeName) -> supervisor:start_child(?MODULE, [NodeName]).

-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

-spec init(_) ->
  {ok,
   {{simple_one_for_one, 5, 10},
    [{slave, {gadget_slave, start_link, []},
      transient, 5000, worker, [gadget_slave]}]}}.
init(_) ->
  {ok,
   {{simple_one_for_one, 5, 10},
    [{gadget_slave, {gadget_slave, start_link, []},
      transient, 5000, worker, [gadget_slave]}]}}.

