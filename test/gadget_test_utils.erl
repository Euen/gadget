-module(gadget_test_utils).

-export([api_call/3]).
-export([api_call/4]).
-export([api_call/5]).
-export([get_github_client_secret/0]).

-type method()  :: atom().
-type url()     :: string() | binary().
-type headers() :: [tuple()] | map().
-type body()    :: map() | binary() | iodata().
-type options() :: [tuple()].

-spec api_call(Method::method(), Url::url(), Headers::headers()) ->
  {atom(), map()}.
api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, <<>>).

-spec api_call( Method::method()
              , Url::url()
              , Headers::headers()
              , Body::body()
              ) ->
  {atom(), map()}.
api_call(Method, Url, Headers, Body) ->
  api_call(Method, Url, Headers, Body, []).

-spec api_call( Method::method()
              , Url::url()
              , Headers::headers()
              , Body::body()
              , Options::options()
              ) ->
  {atom(), map()}.
api_call(Method, Url, Headers, Body, Options) when is_map(Options) ->
  api_call(Method, Url, Headers, Body, maps:to_list(Options));
api_call(Method, Url, Headers, Body, Options) when is_map(Headers) ->
  api_call(Method, Url, maps:to_list(Headers), Body, Options);
api_call(Method, Url, Headers, Body, Options) ->
  {ok, Port} = application:get_env(cowboy, http_port),
  {ok, HttpHost} = application:get_env(cowboy, http_host),
  URL = HttpHost ++ ":" ++ integer_to_list(Port) ++ Url,
  io:format("Sending ~p request to ~p~nHeaders: ~p~nBody: ~p~nOptions: ~p~n",
            [Method, URL, Headers, Body, Options]),
  {ok, StatusCode, RespHeaders, ClientRef} = hackney:request( Method
                                                            , URL
                                                            , Headers
                                                            , Body
                                                            , Options
                                                            ),
  {ok, RespBody} = hackney:body(ClientRef),
  Response = #{status_code => StatusCode
              , headers => RespHeaders
              , body => RespBody
              },
  {ok, Response}.

-spec get_github_client_secret() -> string().
get_github_client_secret() ->
  application:get_env(gadget, github_client_secret, "").
