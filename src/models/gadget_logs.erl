%%% @doc Github Log Documents
-module(gadget_logs).
-author('euen@inaka.net').

-behaviour(sumo_doc).

-opaque log() ::
  #{ id => integer()
   , tool => string()
   , pull_request => string()
   , description => string()
   , created_at => binary()
   }.
-export_type([log/0]).

-export([new/3]).
-export([id/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,           integer,  [id, not_null, auto_increment])
    , sumo:new_field(tool,         string,   [{length, 255}, not_null])
    , sumo:new_field(pull_request, string,   [{length, 255}, not_null])
    , sumo:new_field(description,  string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    ]).

%% @hidden
-spec sumo_sleep(log()) -> sumo:doc().
sumo_sleep(Log) ->
  #{tool := Tool} = Log,
  Log#{tool => atom_to_list(Tool)}.

%% @hidden
-spec sumo_wakeup(sumo:doc()) -> log().
sumo_wakeup(Doc) ->
  #{tool := Tool} = Doc,
  Doc#{tool => list_to_atom(Tool)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a new log
-spec new(string(), atom(), string()) -> log().
new(Tool, Pr, Description) ->
  #{ tool => Tool
   , pull_request => Pr
   , description => Description
   , created_at => ktn_date:now_human_readable()
   }.

%% @doc retrieves the log id
-spec id(log()) -> string().
id(#{id := Token}) -> Token.
