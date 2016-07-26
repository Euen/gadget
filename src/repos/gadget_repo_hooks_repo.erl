%%% @doc Github Repositories Hooks
-module(gadget_repo_hooks_repo).

-export([register/3,
         unregister/1,
         all/0,
         fetch/1,
         fetch_by_repo_id/1]).

%% @doc adds a repo to the database
-spec register(Id::gadget_repo_hooks:id(),
               RepoId::gadget_repo_hooks:repo_id(),
               Url::gadget_repo_hooks:url()) ->
  gadget_repo_hooks:repo_hook().
register(Id, RepoId, Url) ->
  sumo:persist(
    gadget_repo_hooks,
    gadget_repo_hooks:new(
      Id,
      RepoId,
      Url
    )
  ).

%% @doc removes a repo from the database
-spec unregister(Id::gadget_repo_hooks:id()) ->
  non_neg_integer().
unregister(Id) ->
  sumo:delete_by(gadget_repo_hooks, [{id, Id}]).

%% @doc returns all registered repos
-spec all() ->
  [gadget_repo_hooks:repo_hook()].
all() -> sumo:find_all(gadget_repo_hooks).

%% @doc returns a particular repo, if found
-spec fetch(Id::gadget_repo_hooks:id()) ->
  notfound | gadget_repo_hooks:repo_hook().
fetch(Id) ->
  case sumo:find_by(gadget_repo_hooks, [{id, Id}]) of
    [] -> notfound;
    [RepoHook | _] -> RepoHook
  end.

%% @doc returns all the hooks for the given repo
-spec fetch_by_repo_id(RepoId::gadget_repo_hooks:repo_id()) ->
  [gadget_repo_hooks:repo_hook()].
fetch_by_repo_id(RepoId) ->
  case sumo:find_by(gadget_repo_hooks, [{repo_id, RepoId}]) of
    [] -> [];
    RepoHooks -> RepoHooks
  end.
