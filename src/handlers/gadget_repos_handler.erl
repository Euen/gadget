%%% @doc GET /repos handler
-module(gadget_repos_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  case get_user(Req) of
    {ok, User, Cred} ->
      Name = maps:get(<<"name">>, User, null),
      Username =
        case Name of
          null -> maps:get(<<"login">>, User);
          Name1 -> Name1
        end,
      Repos = gadget_core:repositories(Cred),
      WebhookMap = application:get_env(gadget, webhooks, #{}),
      Tools = maps:keys(WebhookMap),
      Variables = [ {tools, Tools}
                  , {user,  Username}
                  , {repos, Repos}
                  ],

      Headers = [{<<"content-type">>, <<"text/html">>}],
      {ok, Body} = repos_dtl:render(Variables),
      {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
      {ok, Req2, State};
    not_found ->
      Headers = [{<<"Location">>, <<"/">>}],
      {ok, Req2} = cowboy_req:reply(302, Headers, Req),
      {ok, Req2, State};
    unauthorized ->
      Headers = [{<<"location">>, <<"/login">>}],
      {ok, Req2} = cowboy_req:reply(302, Headers, Req),
      {ok, Req2, State}
  end.

get_user(Req) ->
  case cowboy_req:cookie(<<"token">>, Req, undefined) of
    {undefined, _} -> not_found;
    {Token, _} ->
      Cred = egithub:oauth(Token),
      case egithub:user(Cred) of
        {ok, User} -> {ok, User, Cred};
        {error, {"401", _, _}} -> unauthorized
      end
  end.

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.
