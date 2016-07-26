%%% @doc Github Repositories Repository
-module(gadget_repos_repo).

-export([register/11,
         unregister/1,
         all/0,
         fetch/1,
         fetch_by_full_name/1,
         update/1,
         languages/1,
         update_languages/2]).

%% @doc adds a repo to the database
-spec register(Id::gadget_repos:id(),
               Name::gadget_repos:name(),
               FName::gadget_repos:full_name(),
               HtmlUrl::gadget_repos:html_url(),
               Priv::gadget_repos:private(),
               Admin::gadget_repos:admin(),
               Pull::gadget_repos:pull(),
               Push::gadget_repos:push(),
               LangsUrl::gadget_repos:languages_url(),
               Lang::gadget_repos:language(),
               Langs::gadget_repos:languages()) -> gadget_repos:repo().
register(
  Id, Name, FName, HtmlUrl, Priv, Admin, Pull, Push, LangsUrl, Lang, Langs
) ->
  sumo:persist(
    gadget_repos,
    gadget_repos:new(
      Id, Name, FName, HtmlUrl, Priv, Admin, Pull, Push, LangsUrl, Lang, Langs)
  ).

%% @doc removes a repo from the database
-spec unregister(Id::gadget_repos:id()) -> non_neg_integer().
unregister(Id) ->
  sumo:delete_by(gadget_repos, [{id, Id}]).

%% @doc returns all registered repos
-spec all() -> [gadget_repos:repo()].
all() -> sumo:find_all(gadget_repos).

%% @doc returns a particular repo, if found
-spec fetch(Id::gadget_repos:id()) -> notfound | gadget_repos:repo().
fetch(Id) ->
  case sumo:find_by(gadget_repos, [{id, Id}]) of
    [] -> notfound;
    [Repo|_] -> Repo
  end.

%% @doc returns the repo with the given full_name as value
-spec fetch_by_full_name(FullName::gadget_repos:full_name()) ->
  notfound | gadget_repos:repo().
fetch_by_full_name(FullName) ->
  case sumo:find_by(gadget_repos, [{full_name, FullName}]) of
    [] -> notfound;
    [Repo|_] -> Repo
  end.

%% @doc update repo info
-spec update(Repo::gadget_repos:repo()) -> gadget_repos:repo().
update(Repo) ->
  sumo:persist(gadget_repos, Repo).

%% @doc returns the list of languages (if any) of the given repo
-spec languages(Id::gadget_repos:id()) -> gadget_repos:languages().
languages(Id) ->
  case sumo:find_by(gadget_repos, [{id, Id}]) of
    [] -> [];
    [Repo|_] ->
      gadget_repos:languages(Repo)
  end.

%% @doc update the list of languages for the given repo
-spec update_languages(Id::gadget_repos:id(),
                       Languages::gadget_repos:languages()) ->
  ok.
update_languages(Id, Languages) ->
  case fetch(Id) of
    notfound ->
      _ =
        lager:warning("Repo with id ~p not found (languages list not updated)",
                      [Id]),
      ok;
    Repo ->
      _NewRepo = update(gadget_repos:languages(Repo, Languages)),
      ok
  end.
