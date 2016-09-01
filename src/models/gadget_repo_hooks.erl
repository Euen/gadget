-module(gadget_repo_hooks).

-behaviour(sumo_doc).

-export([new/3,
         %% Getters
         id/1,
         repo_id/1,
         url/1
        ]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%% Types

-type id()         :: integer().
-type repo_id()    :: integer().
-type url()        :: binary().
-type created_at() :: datetime().
-type updated_at() :: datetime().


-opaque repo_hook() ::
  #{id => id(),
    repo_id => repo_id(),
    url => url(),
    created_at => created_at(),
    updated_at => updated_at()}.

-export_type([repo_hook/0,
              id/0,
              repo_id/0,
              url/0]).

-type datetime() ::
          {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()}
          }.
-export_type([datetime/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(),
          RepoId::repo_id(),
          Url::url()) ->
  repo_hook().
new(Id, RepoId, Url) ->
  Now = gadget_utils:now_datetime(),
  #{id => Id,
    repo_id => RepoId,
    url => Url,
    created_at => Now,
    updated_at => Now
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Getters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec id(RepoHook::repo_hook()) -> integer().
id(RepoHook) ->
  maps:get(id, RepoHook).

-spec repo_id(RepoHook::repo_hook()) -> repo_id().
repo_id(RepoHook) ->
  maps:get(repo_id, RepoHook).

-spec url(RepoHook::repo_hook()) -> url().
url(RepoHook) ->
  maps:get(url, RepoHook).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,            integer,  [id, not_null]),
     sumo:new_field(repo_id,       integer,  [not_null]),
     sumo:new_field(url,           string,   [{length, 255}]),
     sumo:new_field(created_at,    datetime, [not_null]),
     sumo:new_field(updated_at,    datetime, [not_null])
    ],
  sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(RepoHook::repo_hook()) -> sumo:doc().
sumo_sleep(RepoHook) ->
  RepoHook.

-spec sumo_wakeup(Doc::sumo:doc()) -> repo_hook().
sumo_wakeup(Doc) ->
  Doc.
