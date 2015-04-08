-module(gadget_repos_repo).
-author('elbrujohalcon@inaka.net').

-export([register/3, unregister/2, all/0, fetch/2]).

-spec register(string(), atom(), string()) -> gadget_repos:repo().
register(Name, Tool, Token) ->
  sumo:persist(gadget_repos, gadget_repos:new(Name, Tool, Token)).

-spec unregister(string(), atom()) -> non_neg_integer().
unregister(Name, Tool) ->
  sumo:delete_by(gadget_repos, [{name, Name}, {tool, atom_to_list(Tool)}]).

-spec all() -> [gadget_repos:repo()].
all() -> sumo:find_all(gadget_repos).

-spec fetch(string(), atom()) -> notfound | gadget_repos:repo().
fetch(Name, Tool) ->
  case sumo:find_by(gadget_repos, [{name, Name}, {tool, atom_to_list(Tool)}]) of
    [] -> notfound;
    [Repo|_] -> Repo
  end.
