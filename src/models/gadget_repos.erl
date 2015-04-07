-module(gadget_repos).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque repo() ::
  #{
    id => integer(),
    name => string(),
    token => string(),
    created_at => binary()
   }.
-export_type([repo/0]).

-export([new/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          integer,  [id, not_null, auto_increment])
    , sumo:new_field(name,        string,   [{length, 255}, not_null])
    , sumo:new_field(token,       string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(repo()) -> sumo:doc().
sumo_sleep(Repo) -> Repo.

-spec sumo_wakeup(sumo:doc()) -> repo().
sumo_wakeup(Doc) -> Doc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new(string(), string()) -> repo().
new(Name, Token) ->
  #{ name => Name
   , token => Token
   , created_at => ktn_date:now_human_readable()
   }.
