-module(gadget_sup).
-behavior(supervisor).

-export([
         start_link/0
        ]).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, {}).

%%% Supervisor Behavior

-spec init(term()) -> term().
init({}) ->
    {
      ok,
      {
        {one_for_one, 5, 10},
        []
      }}.
