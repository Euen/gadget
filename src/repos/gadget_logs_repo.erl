%%% @doc Github Logs Repository
-module(gadget_logs_repo).
-author('euen@inaka.net').

-export([all/0, fetch/1, create/4]).

%% @doc create a new Log
-spec create(atom(), string(), integer(), string()) -> gadget_logs:log().
create(Tool, Repo, PrNumber, Description) ->
  Log = gadget_logs:new(Tool, Repo, PrNumber, Description),
  sumo:persist(gadget_logs, Log).

%% @doc returns all logs
-spec all() -> [gadget_logs:log()].
all() -> sumo:find_all(gadget_logs).

%% @doc returns a particular log
-spec fetch(integer()) -> notfound | gadget_logs:log().
fetch(Id) ->
  sumo:find(gadget_logs, Id).
