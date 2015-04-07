-module(gadget_repos_repo).
-author('elbrujohalcon@inaka.net').

-export([register/2, unregister/1, all/0]).

-spec register(string(), string()) -> gadget_repos:repo().
register(Name, Token) ->
  sumo:persist(gadget_repos, gadget_repos:new(Name, Token)).

-spec unregister(string()) -> non_neg_integer().
unregister(Name) ->
  sumo:delete_by(gadget_repos, [{name, Name}]).

-spec all() -> [gadget_repos:repo()].
all() ->
  sumo:find_all(gadget_repos).
