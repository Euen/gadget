%%% @doc Github Log Documents
-module(gadget_logs).
-author('euen@inaka.net').

-behaviour(sumo_doc).

-opaque log() ::
  #{ id => integer()
   , tool => string() | binary() | atom()
   , repository => string()
   , pr_number => integer()
   , description => string()
   , back_url => string()
   , created_at => binary()
   }.
-export_type([log/0]).

-export([new/4]).
-export([id/1, repository/1, pr_number/1, back_url/1, back_url/2]).
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
    , sumo:new_field(repository,   string,   [{length, 255}, not_null])
    , sumo:new_field(pr_number,    integer,  [not_null])
    , sumo:new_field(description,  string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,   datetime, [not_null])
    ]).

%% @hidden
-spec sumo_sleep(log()) -> sumo:doc().
sumo_sleep(Log) ->
  #{tool := Tool} = Log,
  Log#{tool => atom_to_binary(Tool, utf8)}.

%% @hidden
-spec sumo_wakeup(sumo:doc()) -> log().
sumo_wakeup(Doc) ->
  #{tool := Tool} = Doc,
  Doc#{tool => binary_to_atom(Tool, utf8)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a new log
-spec new(atom(), string(), integer(), string()) -> log().
new(Tool, Repo, Pr, Description) ->
  #{ tool => Tool
   , repository => Repo
   , pr_number => Pr
   , description => Description
   , back_url => ""
   , created_at => ktn_date:now_human_readable()
   }.

%% @doc retrieves the log id
-spec id(log()) -> integer().
id(#{id := Id}) -> Id.

%% @doc retrieves the log repository
-spec repository(log()) -> string().
repository(#{repository := Repo}) -> Repo.

%% @doc retrieves the log pr_number
-spec pr_number(log()) -> integer().
pr_number(#{pr_number := PrNumber}) -> PrNumber.

%% @doc retrieves `back_url' from the given `log()'
-spec back_url(log()) -> string().
back_url(#{back_url := BackUrl}) -> BackUrl.

%% @doc set a new value for `back_url' in the given `log()'
-spec back_url(log(), string()) -> log().
back_url(Log, BackUrl) -> Log#{back_url => BackUrl}.
