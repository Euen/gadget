-module(gadget_users).

-behaviour(sumo_doc).

-export([new/3,
         %% Getters
         id/1,
         username/1,
         repo_ids/1
        ]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%% Types

-type id()       :: integer().
-type username() :: binary().
-type repo_ids() :: [integer()].

-opaque user() ::
          #{id => id(),
            username => username(),
            repo_ids => repo_ids(),
            created_at => datetime(),
            updated_at => datetime()
           }.

-export_type([user/0,
              id/0,
              username/0,
              repo_ids/0]).

-type datetime() ::
          {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()}
          }.
-export_type([datetime/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(), UserName::username(), RepoIds::repo_ids()) ->
  user().
new(Id, UserName, RepoIds) ->
  Now = gadget_utils:now_datetime(),
  #{id => Id,
    username => UserName,
    repo_ids => RepoIds,
    created_at => Now,
    updated_at => Now
   }.

%% Getters
-spec id(User::user()) -> integer().
id(User) ->
  maps:get(id, User).

-spec username(User::user()) -> username().
username(User) ->
  maps:get(username, User).

-spec repo_ids(User::user()) -> repo_ids().
repo_ids(User) ->
  maps:get(repo_ids, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,            integer,  [id, not_null]),
     sumo:new_field(username,      string,   [{length, 255}]),
     sumo:new_field(repo_ids,      binary,   [{length, 255}]),
     sumo:new_field(created_at,    datetime, [not_null]),
     sumo:new_field(updated_at,    datetime, [not_null])
    ],
  sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(User::user()) -> sumo:doc().
sumo_sleep(User) ->
  User#{repo_ids => term_to_binary(maps:get(repo_ids, User))}.

-spec sumo_wakeup(Doc::sumo:doc()) -> user().
sumo_wakeup(Doc) ->
  Doc#{repo_ids => binary_to_term(maps:get(repo_ids, Doc))}.
