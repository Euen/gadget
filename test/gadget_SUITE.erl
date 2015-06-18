
-module(gadget_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_status/1]).
-export([test_about/1]).
-export([test_login/1]).




-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%

-spec ignored_funs() -> [atom()].
ignored_funs() ->
  [ module_info
  , init_per_suite
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(mnesia),
  application:ensure_all_started(cowboy),
  application:ensure_all_started(sumo_db),
  application:ensure_all_started(gadget),
  sumo:create_schema(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%% @doc definion of init_per_testcases

init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases

end_per_testcase(_Function, Config) ->
  Config.

-spec test_status(config()) -> config().
test_status(Config) ->

  Header = #{<<"Content-Type">> => <<"text/plain; charset=utf-8">>},
  {ok, Response} =
  gadget_test_utils:api_call(get, "/status", Header),
  #{status_code := 200, body := <<"Server is Up">>} = Response,
  Config.

-spec test_about(config()) -> config().
test_about(Config) ->
  Header = #{<<"Content-Type">> => <<"text/plain; charset=utf-8">>},
  {ok, Response} =
  gadget_test_utils:api_call(get, "/about", Header),
  #{status_code := 200} = Response,
  Config.

-spec test_login(config()) -> config().
test_login(Config) ->
  Header = #{<<"Content-Type">> => <<"text/plain; charset=utf-8">>},
  {ok, Response} =
  gadget_test_utils:api_call(get, "/login", Header),
  #{status_code := 302} = Response,
  Config.

%%-spec test_status(config()) -> config().
%%test_status(Config) ->
%%
%%  Header = #{ <<"Content-Type">> => <<"application/json">>},
%%  Body = #{},
%%  JsonBody = jiffy:encode(Body),
%%  {ok, Response} =
%%  cnf_test_utils:api_call(get, "/status", Header,  JsonBody),
%%  #{status_code := 204} = Response,
%%  #{headers := ResponseHeaders} = Response,
%%  Location = proplists:get_value(<<"location">>, ResponseHeaders),
%%  <<"http://localhost/contents/", _Id/binary>> = Location,
%%  Config.
%%  ok.
%%
