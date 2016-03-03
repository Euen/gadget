%%% @doc Github Logs Repository
-module(gadget_logs_repo).
-author('euen@inaka.net').

-export([all/0, fetch/1, create/3]).

%% @doc create a new Log
-spec create(string(), atom(), string()) -> gadget_logs:log().
create(Tool, Pr, Description) ->
  Log = gadget_logs:new(Tool, Pr, Description),
  sumo:persist(gadget_logs, Log).

%% @doc returns all logs
-spec all() -> [gadget_repos:repo()].
all() -> sumo:find_all(gadget_logs).

%% @doc returns a particular log
-spec fetch(integer()) -> notfound | gadget_logs:log().
fetch(Id) ->
  sumo:find(gadget_logs, Id).
