-module(gadget_repos_repo).
-author('elbrujohalcon@inaka.net').

-export([register/3, unregister/2, all/0]).

-spec register(string(), atom(), string()) -> gadget_repos:repo().
register(Name, Tool, Token) ->
  sumo:persist(gadget_repos, gadget_repos:new(Name, Tool, Token)).

-spec unregister(string(), atom()) -> non_neg_integer().
unregister(Name, Tool) ->
  sumo:delete_by(gadget_repos, [{name, Name}, {tool, Tool}]).

-spec all() -> [gadget_repos:repo()].
all() -> sumo:find_all(gadget_repos).
