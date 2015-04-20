%%% @doc Main app supervisor
-module(gadget_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% @private
-spec init(term()) -> term().
init({}) ->
  {ok,
    { {one_for_one, 5, 10}
    , [ { gadget_slave_sup
        , {gadget_slave_sup, start_link, []}
        , permanent, 1000, supervisor, [gadget_slave_sup]
        }
      ]
    }}.
