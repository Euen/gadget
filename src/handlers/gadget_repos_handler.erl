%%% @doc GET /repos handler
-module(gadget_repos_handler).
-ignore_xref([{repos_dtl, render, 1}]).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  case get_user(Req) of
    {ok, User, Cred} ->
      {Filter, _} = cowboy_req:qs_val(<<"filter">>, Req),
      {Sync, _} = cowboy_req:qs_val(<<"sync">>, Req),
      RegisteredUser = gadget_users_repo:fetch(maps:get(<<"id">>, User)),
      Username = gadget_users:username(RegisteredUser),
      Repos = get_or_sync_user_repos(Sync, Cred, RegisteredUser),
      FilteredRepos = gadget_core:filter_repositories(Filter, Repos),
      SortedRepos = lists:usort([gadget_core:repo_info(Repo, Filter) ||
                                 Repo <- FilteredRepos]),
      WebhookMap = application:get_env(gadget, webhooks, #{}),
      Tools = maps:keys(WebhookMap),
      Variables = [ {tools, Tools}
                  , {user,  Username}
                  , {repos, SortedRepos}
                  , {filter, Filter}
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

%% @private
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) -> ok.

%% =============================================================================
%% Private
%% =============================================================================

%% @private
get_user(Req) ->
  case cowboy_req:cookie(<<"token">>, Req, undefined) of
    {undefined, _} -> not_found;
    {Token, _} ->
      Cred = egithub:oauth(Token),
      case egithub:user(Cred) of
        {ok, User} ->
          ok = maybe_create_user(User),
          {ok, User, Cred};
        {error, {"401", _, _}} ->
          unauthorized
      end
  end.

-spec get_or_sync_user_repos(Sync::binary(),
                             Cred::egithub:credentials(),
                             RegUser::gadget_users:user()) ->
  [gadget_repos:repo()].
get_or_sync_user_repos(Sync, Cred, RegUser) ->
  case Sync of
    <<"1">> ->
      _ = lager:info("Fetching repos from GitHub...", []),
      gadget_core:sync_repositories(Cred, RegUser);
    _ ->
      % Try to get all the repos associated to the requester user
      case gadget_users_repo:repos(gadget_users:id(RegUser)) of
        [] ->
          _ = lager:info("Fetching repos from GitHub...", []),
          gadget_core:sync_repositories(Cred, RegUser);
        FoundRepos ->
          _ = lager:info("Fetching repos from DB (cache)...", []),
          FoundRepos % cache
      end
  end.

-spec maybe_create_user(User::map()) ->
  ok.
maybe_create_user(User) ->
  % Create a new user in the database if it doesn't exist yet
  case gadget_users_repo:fetch(maps:get(<<"id">>, User)) of
    notfound ->
      UserName = gadget_utils:get_username(User),
      _ = gadget_users_repo:register(
        maps:get(<<"id">>, User),
        UserName,
        []
       ),
      ok;
    _FoundUser ->
      ok
  end.
