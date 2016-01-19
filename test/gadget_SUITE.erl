
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
-export([ valid_organization_repositories_test/1
        , valid_organization_payload_test/1
        , invalid_organization_payload_test/1]).

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

-spec init_per_testcase(TestCase::atom(), Config::config()) -> config().
init_per_testcase(valid_organization_repositories_test, Config) ->
  ok = meck:new(egithub, [passthrough]),
  ok = meck:expect(egithub, orgs, fun egithub_orgs/1),
  ok = meck:expect(egithub, all_org_repos, fun egithub_all_org_repos/3),
  ok = meck:expect(egithub, hooks, fun egithub_hooks/2),
  Config;
init_per_testcase(_TestCase, Config) ->
  Config.

%% @doc definion of end_per_testcases

-spec end_per_testcase(TestCase::atom(), Config::config()) -> config().
end_per_testcase(valid_organization_repositories_test, Config) ->
  ok = meck:unload(egithub),
  Config;
end_per_testcase(_TestCase, Config) ->
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

-spec basic_test(atom(), config()) -> config().
basic_test(Webhook, Config) ->
  Header =
    #{  <<"Content-Type">> => <<"application/json">>
      , <<"x-github-event">> =>  <<"ping">>},
  Token = gadget_test_utils:get_github_client_secret(),
  _ = gadget_repos_repo:register("gadget-tester/user-repo", Webhook, Token),
  {ok, JsonBody} =
    file:read_file("../../test/github_payloads/initial-payload.json"),
  {ok, Response} =
    gadget_test_utils:api_call(get, "/webhook/compiler/", Header, JsonBody),
  % Given payload does not have an action key because it is the initial payload
  % sent by GitHub after you register a webhook, so it is ignored by gadget.
  #{ status_code := 200, body := <<"Event ignored.">>} = Response,
  Config.

-spec valid_organization_repositories_test(Config::config()) -> config().
valid_organization_repositories_test(Config) ->
  [Repositories | _] = gadget_core:repositories({'oauth', "mycredentials"}),
  <<"inaka/harry">> = proplists:get_value(full_name, Repositories),
  Config.

-spec valid_organization_payload_test(Config::config()) -> config().
valid_organization_payload_test(Config) ->
  Header =
    #{  <<"Content-Type">> => <<"application/json">>
      , <<"x-github-event">> =>  <<"pull_request">>},
  PayloadPath = "../../test/github_payloads/valid_organization_payload.json",
  {ok, JsonBody} = file:read_file(PayloadPath),
  {ok, Response} =
    gadget_test_utils:api_call(get, "/webhook/elvis/", Header, JsonBody),
  #{ status_code := 200, body := <<"Event processed.">>} = Response,
  Config.

-spec invalid_organization_payload_test(Config::config()) -> config().
invalid_organization_payload_test(Config) ->
  Header =
    #{  <<"Content-Type">> => <<"application/json">>
      , <<"x-github-event">> =>  <<"pull_request">>},
  PayloadPath = "../../test/github_payloads/invalid_organization_payload.json",
  {ok, JsonBody} = file:read_file(PayloadPath),
  {ok, Response} =
    gadget_test_utils:api_call(get, "/webhook/compiler/", Header, JsonBody),
  #{ status_code := 403, body := <<"Event not  processed.">>} = Response,
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec egithub_orgs(Cred::tuple()) -> {ok, [map()]}.
egithub_orgs(_Cred) ->
  {ok, [Orgs | _]} = file:consult("../../test/meck_data/egithub__orgs.txt"),
  {ok, Orgs}.

-spec egithub_all_org_repos(Cred::tuple, OrgName::binary(), Opts::map()) ->
  {ok, [map()]}.
egithub_all_org_repos(_Cred, <<"inaka">>, _Opts) ->
  {ok, [OrgRepos | _]} =
    file:consult("../../test/meck_data/egithub__all_org_repos.txt"),
  {ok, OrgRepos};
egithub_all_org_repos(_Cred, _OrgName, _Opts) -> {ok, []}.

-spec egithub_hooks(Cred::tuple(), FullName::binary()) -> {ok, [map()]}.
egithub_hooks(_Cred, _FullName) ->
  {ok, [Hooks | _]} = file:consult("../../test/meck_data/egithub__hooks.txt"),
  {ok, Hooks}.
