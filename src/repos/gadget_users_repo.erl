-module(gadget_users_repo).

-export([register/3, unregister/1, all/0, fetch/1, repos/1]).

%% @doc adds a user to the database
-spec register(Id::gadget_users:id(),
               UserName::gadget_users:username(),
               RepoIds::gadget_users:repo_ids()) -> gadget_users:user().
register(Id, UserName, RepoIds) ->
  sumo:persist(
    gadget_users,
    gadget_users:new(
      Id,
      UserName,
      RepoIds
    )
  ).

%% @doc removes a user from the database
-spec unregister(Id::gadget_users:id()) -> non_neg_integer().
unregister(Id) ->
  sumo:delete_by(gadget_users, [{id, Id}]).

%% @doc returns all registered users
-spec all() -> [gadget_users:user()].
all() -> sumo:find_all(gadget_users).

%% @doc returns a particular user, if found
-spec fetch(Id::gadget_users:id()) -> notfound | gadget_users:user().
fetch(Id) ->
  case sumo:find_by(gadget_users, [{id, Id}]) of
    [] -> notfound;
    [User | _] -> User
  end.

%% @doc returns the info for all the repositories associated to this user
-spec repos(Id::gadget_users:id()) -> [gadget_repos:repo()].
repos(Id) ->
  case sumo:find_by(gadget_users, [{id, Id}]) of
    [] ->
      [];
    [User | _] ->
      RepoIds = gadget_users:repo_ids(User),
      [gadget_repos_repo:fetch(RepoId) || RepoId <- RepoIds]
  end.
