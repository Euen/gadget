-module(gadget_repos_handler).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, Req, State} | {shutdown, Req, State}.
init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

-spec handle(cowboy_req:req(), #state{}) -> ok.
handle(Req, State) ->
  case cowboy_req:cookie(<<"token">>, Req, undefined) of
    {undefined, _} ->
      Headers = [{<<"Location">>, <<"/">>}],
      {ok, Req2} = cowboy_req:reply(302, Headers, Req),
      {ok, Req2, State};
    {Token, _} ->
      Cred = egithub:oauth(Token),
      {ok, User} = egithub:user(Cred),
      Name = maps:get(<<"name">>, User),
      Username =
        case Name of
          null -> maps:get(<<"login">>, User);
          Name1 -> Name1
        end,
      Repos = repositories(Cred),
      WebhookMap = application:get_env(gadget, webhooks, #{}),
      Tools = maps:keys(WebhookMap),
      Variables = [ {tools, Tools}
                  , {user,  Username}
                  , {repos, Repos}
                  ],

      Headers = [{<<"content-type">>, <<"text/html">>}],
      {ok, Body} = repos_dtl:render(Variables),
      {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
      {ok, Req2, State}
  end.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Returns a proplist with all user's repos and their status.
repositories(Cred) ->
  Opts = #{type => <<"owner">>},
  {ok, Repos} = egithub:all_repos(Cred, Opts),
  {ok, Orgs} = egithub:orgs(Cred),

  OrgReposFun =
    fun(#{<<"login">> := OrgName}) ->
      {ok, OrgRepos} = egithub:all_org_repos(Cred, OrgName, Opts),
      OrgRepos
    end,
  AllOrgsRepos = lists:flatmap(OrgReposFun, Orgs),

  PublicRepos =
    [Repo || Repo <- Repos ++ AllOrgsRepos,
             gadget_utils:is_admin(Repo), gadget_utils:is_public(Repo)],

  lists:sort([repo_info(Cred, Repo) || Repo <- PublicRepos]).

repo_info(Cred, Repo) ->
  Name = maps:get(<<"name">>, Repo),
  FullName = maps:get(<<"full_name">>, Repo),
  HtmlUrl = maps:get(<<"html_url">>, Repo),
  Hooks =
    case egithub:hooks(Cred, FullName) of
      {ok, HooksResult} -> HooksResult;
      {error, _} -> []
    end,

  WebhookMap = application:get_env(gadget, webhooks, #{}),

  Status = gadget_utils:enabled_tools(WebhookMap, Hooks),
  StatusList = lists:map(fun maps:to_list/1, Status),
  [ {full_name, FullName}
  , {name, Name}
  , {html_url, HtmlUrl}
  , {status, StatusList}
  ].
