%%% @doc Github Repositories-Tools Repository
-module(gadget_repo_tools_repo).
-author('elbrujohalcon@inaka.net').

-export([register/3, unregister/2, all/0, fetch/2]).

%% @doc adds a repo-tool to the database
-spec register(string(), atom(), string()) -> gadget_repo_tools:repo_tool().
register(Name, Tool, Token) ->
  sumo:persist(gadget_repo_tools, gadget_repo_tools:new(Name, Tool, Token)).

%% @doc removes a repo-tool from the database
-spec unregister(string(), atom()) -> non_neg_integer().
unregister(Name, Tool) ->
  sumo:delete_by(
    gadget_repo_tools, [{name, Name}, {tool, atom_to_binary(Tool, utf8)}]).

%% @doc returns all registered repo-tools
-spec all() -> [gadget_repo_tools:repo_tool()].
all() -> sumo:find_all(gadget_repo_tools).

%% @doc returns a particular repo-tool, if found
-spec fetch(string(), atom()) -> notfound | gadget_repo_tools:repo_tool().
fetch(Name, Tool) ->
  Conditions = [{name, Name}, {tool, atom_to_binary(Tool, utf8)}],
  case sumo:find_by(gadget_repo_tools, Conditions) of
    [] -> notfound;
    [Repo|_] -> Repo
  end.
