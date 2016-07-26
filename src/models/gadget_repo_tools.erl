%%% @doc Github Repository-Tools Documents
-module(gadget_repo_tools).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque repo_tool() ::
  #{ id => integer()
   , name => string()
   , tool => atom()
   , token => string()
   , created_at => binary()
   }.
-export_type([repo_tool/0]).

-export([new/3]).
-export([token/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          integer,  [id, not_null, auto_increment])
    , sumo:new_field(tool,        string,   [{length, 255}, not_null])
    , sumo:new_field(name,        string,   [{length, 255}, not_null])
    , sumo:new_field(token,       string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    ]).

%% @hidden
-spec sumo_sleep(repo_tool()) -> sumo:doc().
sumo_sleep(Repo) ->
  #{tool := Tool} = Repo,
  Repo#{tool => atom_to_binary(Tool, utf8)}.

%% @hidden
-spec sumo_wakeup(sumo:doc()) -> repo_tool().
sumo_wakeup(Doc) ->
  #{tool := Tool} = Doc,
  Doc#{tool => binary_to_atom(Tool, utf8)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a new repository
-spec new(string(), atom(), string()) -> repo_tool().
new(Name, Tool, Token) ->
  #{ name => Name
   , tool => Tool
   , token => Token
   , created_at => ktn_date:now_human_readable()
   }.

%% @doc retrieves the repository token
-spec token(repo_tool()) -> string().
token(#{token := Token}) -> Token.
