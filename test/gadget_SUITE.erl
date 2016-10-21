
-module(gadget_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_status/1]).
-export([test_about/1]).
-export([test_login/1]).
-export([test_elvis/1]).
-export([test_compiler/1]).
-export([test_xref/1]).
-export([test_dialyzer/1]).
-export([test_lewis/1]).
-export([github_fail_with_compiler/1]).
-export([github_fail_with_dialyzer/1]).
-export([github_fail_with_elvis/1]).
-export([github_fail_with_lewis/1]).
-export([github_fail_with_xref/1]).

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
  _ = application:stop(mnesia),
  _ = mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = application:ensure_all_started(sumo_db),
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(gadget),
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

-spec test_elvis(config()) -> config().
test_elvis(Config) ->
  basic_test(elvis, Config).

-spec test_dialyzer(config()) -> config().
test_dialyzer(Config) ->
  basic_test(dialyzer, Config).

-spec test_xref(config()) -> config().
test_xref(Config) ->
  basic_test(xref, Config).

-spec test_compiler(config()) -> config().
test_compiler(Config) ->
  basic_test(compiler, Config).

-spec test_lewis(config()) -> config().
test_lewis(Config) ->
  basic_test(lewis, Config).

-spec basic_test(atom(), config()) -> config().
basic_test(Webhook, Config) ->
  Header =
    #{  <<"Content-Type">> => <<"application/json">>
      , <<"x-github-event">> =>  <<"ping">>},
  Token = list_to_binary(gadget_test_utils:get_github_client_secret()),
  _ = gadget_repo_tools_repo:register( <<"gadget-tester/user-repo">>
                                     , Webhook
                                     , Token),
  {ok, JsonBody} =
    file:read_file("../../priv/initial-payload.json"),
  {ok, Response} =
    gadget_test_utils:api_call(get, "/webhook/compiler/", Header, JsonBody),
  #{ status_code := 200, body := <<"Event processed.">>} = Response,
  Config.

-spec github_fail_with_compiler(config()) -> ok.
github_fail_with_compiler(_Config) ->
  github_fail_with_tool(gadget_compiler_webhook).

-spec github_fail_with_dialyzer(config()) -> ok.
github_fail_with_dialyzer(_Config) ->
  github_fail_with_tool(gadget_dialyzer_webhook).

-spec github_fail_with_elvis(config()) -> ok.
github_fail_with_elvis(_Config) ->
  github_fail_with_tool(gadget_elvis_webhook).

-spec github_fail_with_lewis(config()) -> ok.
github_fail_with_lewis(_Config) ->
  github_fail_with_tool(gadget_lewis_webhook).

-spec github_fail_with_xref(config()) -> ok.
github_fail_with_xref(_Config) ->
  github_fail_with_tool(gadget_xref_webhook).

-spec github_fail_with_tool(atom()) -> ok.
github_fail_with_tool(Tool) ->
  {ok, [RequestMap]} = file:consult("../../test/request_map.txt"),
  ToolName = <<"compiler">>,
  meck:new(egithub, [passthrough]),
  meck:new(Tool, [passthrough]),

  {ok, GithubFiles} = file:consult("../../test/github_files.txt"),
  PullReqFiles = fun(_, _, _) -> {ok, GithubFiles} end,
  meck:expect(egithub, pull_req_files, PullReqFiles),

  {ok, [Error]} = file:consult("../../test/event_error.txt"),
  ErrorFun = fun(_, _, _) -> throw(Error) end,
  meck:expect(Tool, handle_pull_request, ErrorFun),

  try
    ok = gadget:webhook(ToolName, RequestMap)
  catch
    _:{badmatch,{error,{404, _, _}}} -> ok
  after
    meck:unload(egithub),
    meck:unload(Tool)
  end.