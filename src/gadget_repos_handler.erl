-module(gadget_repos_handler).

-export([
         init/3,
         handle/2,
         terminate/3
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
            lager:info("Token:~p", [Token]),
            Cred = egithub:oauth(Token),
            {ok, User} = egithub:user(Cred),
            Name = maps:get(<<"name">>, User),
            Username =
                case Name of
                    null -> maps:get(<<"login">>, User);
                    Name1 -> Name1
                end,
            {ok, Repos} = repositories(Cred),
            Variables = [{user, Username},
                         {repos, Repos}],

            Headers = [{<<"content-type">>, <<"text/html">>}],
            {ok, Body} = repos_dtl:render(Variables),
            {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Req2, State}
    end.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

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
        fun (#{<<"login">> := OrgName}) ->
            {ok, OrgRepos} = egithub:all_org_repos(Cred, OrgName, Opts),
            case is_owners_member(Cred, OrgName) of
                true ->
                    OrgRepos;
                false ->
                    lists:filter(fun gadget_utils:is_public_repo/1,
                                 OrgRepos)
            end
        end,

    FilterAdmin =
        fun
            (#{<<"permissions">> := #{<<"admin">> := true}}) ->
                true;
            (_Repo) ->
                false
        end,
    SortFun =
        fun
            (#{<<"full_name">> := A},
             #{<<"full_name">> := B}) ->
                A < B
        end,

    AllOrgsRepos = lists:flatmap(OrgReposFun, Orgs),
    AdminRepos = lists:filter(FilterAdmin, Repos ++ AllOrgsRepos),
    AllRepos = lists:sort(SortFun, AdminRepos),

    Fun = fun(Repo) -> repo_info(Cred, Repo) end,
    {ok, lists:map(Fun, AllRepos)}.

repo_info(Cred, Repo) ->
    Name = maps:get(<<"name">>, Repo),
    FullName = maps:get(<<"full_name">>, Repo),
    HtmlUrl = maps:get(<<"html_url">>, Repo),
    Hooks = case egithub:hooks(Cred, FullName) of
                {ok, HooksResult} ->
                    HooksResult;
                {error, _} ->
                    []
            end,
    WebhookMap = application:get_env(gadget, webhooks),
    ElvisWebhook = maps:get("elvis", WebhookMap),
    WhkUrl = maps:get("url", ElvisWebhook),
    Status = case gadget_utils:hook_by_url(binary_to_list(WhkUrl), Hooks) of
                 not_found -> off;
                 _ -> on
             end,
    [{name, Name},
     {full_name, FullName},
     {html_url, HtmlUrl},
     {status, Status}].

is_owners_member(Cred, OrgName) ->
    {ok, Teams} = egithub:teams(Cred, OrgName),
    [TeamId] = [TeamId || #{<<"id">> := TeamId, <<"name">> := Name} <- Teams,
                          <<"Owners">> == Name],
    {ok, #{<<"login">> := Username}} = egithub:user(Cred),

    active == egithub:team_membership(Cred, TeamId, Username).
