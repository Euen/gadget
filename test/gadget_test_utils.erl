-module(gadget_test_utils).

-export([api_call/3]).
-export([api_call/4]).
-export([api_call/5]).
-export([get_github_client_secret/0]).


-spec api_call(atom(), string(), map()) -> {atom(), map()}.
api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, "").

-spec api_call(atom(), string(), map(), map() | iodata()) -> {atom(), map()}.
api_call(Method, Url, Headers, Body) ->
  {ok, Port} = application:get_env(cowboy, http_port),
  {ok, HttpHost} = application:get_env(cowboy, http_host),
  {ok, Pid} = shotgun:open(HttpHost, Port),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, #{} ),
  shotgun:close(Pid),
  Response.

-spec api_call(atom(), string(), map(), map() | iodata(), map()) ->
  {atom(), map()}.
api_call(Method, Url, Headers, Body, Option) ->
  {ok, Port} = application:get_env(cowboy, http_port),
  {ok, HttpHost} = application:get_env(cowboy, http_host),
  {ok, Pid} = shotgun:open(HttpHost, Port),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, Option),
  shotgun:close(Pid),
  Response.


-spec get_github_client_secret() -> string().
get_github_client_secret() ->
  application:get_env(gadget, github_client_secret, "").
