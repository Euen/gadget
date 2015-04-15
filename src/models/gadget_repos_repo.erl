%%% @doc Github Repositories Repository
-module(gadget_repos_repo).
-author('elbrujohalcon@inaka.net').

-export([register/3, unregister/2, all/0, fetch/2]).

%% @doc adds a repo to the database
-spec register(string(), atom(), string()) -> gadget_repos:repo().
register(Name, Tool, Token) ->
  sumo:persist(gadget_repos, gadget_repos:new(Name, Tool, Token)).

%% @doc removes a repo from the database
-spec unregister(string(), atom()) -> non_neg_integer().
unregister(Name, Tool) ->
  sumo:delete_by(gadget_repos, [{name, Name}, {tool, atom_to_list(Tool)}]).

%% @doc returns all registered repos
-spec all() -> [gadget_repos:repo()].
all() -> sumo:find_all(gadget_repos).

%% @doc returns a particular repo, if found
-spec fetch(string(), atom()) -> notfound | gadget_repos:repo().
fetch(Name, Tool) ->
  case sumo:find_by(gadget_repos, [{name, Name}, {tool, atom_to_list(Tool)}]) of
    [] -> notfound;
    [Repo|_] -> Repo
  end.
