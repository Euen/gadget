-module(gadget_repos).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque repo() ::
  #{ id => integer()
   , name => string()
   , tool => atom()
   , token => string()
   , created_at => binary()
   }.
-export_type([repo/0]).

-export([new/3]).
-export([token/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          integer,  [id, not_null, auto_increment])
    , sumo:new_field(tool,        string,   [{length, 255}, not_null])
    , sumo:new_field(name,        string,   [{length, 255}, not_null])
    , sumo:new_field(token,       string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(repo()) -> sumo:doc().
sumo_sleep(Repo) ->
  #{tool := Tool} = Repo,
  Repo#{tool => atom_to_list(Tool)}.

-spec sumo_wakeup(sumo:doc()) -> repo().
sumo_wakeup(Doc) ->
  #{tool := Tool} = Doc,
  Doc#{tool => list_to_atom(Tool)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new(string(), atom(), string()) -> repo().
new(Name, Tool, Token) ->
  #{ name => Name
   , tool => Tool
   , token => Token
   , created_at => ktn_date:now_human_readable()
   }.

-spec token(repo()) -> string().
token(#{token := Token}) -> Token.
