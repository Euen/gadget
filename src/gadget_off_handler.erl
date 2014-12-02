-module(gadget_off_handler).

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         delete_resource/2,
         terminate/3
        ]).

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(term(), cowboy_req:req(), term()) -> 
    {term(), term(), term()}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), term()) -> 
  {ok, cowboy_req:req(), #state{}}.
rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.
-spec allowed_methods(cowboy_req:req(), term()) ->
    {[_], cowboy_req:req(), term()}.
allowed_methods(Req, State) ->
  {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

-spec delete_resource(cowboy_req:req(), #state{}) -> ok.
delete_resource(Req, State) ->
    {ToolNameBin, _} =  cowboy_req:binding(tool, Req),
    {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
    {Repo, _} = cowboy_req:qs_val(<<"repo">>, Req, ""),
    Cred = egithub:oauth(Token),

    {ok, WebhookMap} = application:get_env(gadget, webhooks),
    ToolName = binary_to_atom(ToolNameBin, utf8),
    Tool = maps:get(ToolName, WebhookMap),
    ok = remove_user(Cred, Repo, maps:get(username, Tool)),
    {ok, Hooks} = egithub:hooks(Cred, Repo),
    EnabledTools = gadget_utils:enabled_tools(WebhookMap, Hooks),
    HId =
        [HookId 
        ||  #{hook_id := HookId, 
              name := ToolName1,
              status := Status} <- EnabledTools,
            ToolName1 == ToolName,
            Status == on],
    io:format("~p~n", [HId]),
    case HId of
        [] -> 
            not_found;
        [Id] -> 
            ok = egithub:delete_webhook(Cred, Repo, Id)
    end,
    {true, Req, State}.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Only remove the user if the repo belongs to a user, otherwise the
%%      the user should not be removed from the Services team just in case
%%      it is being used in another project from the same organisation.
remove_user(Cred, Repo, Username) ->
    {ok, RepoInfo} = egithub:repo(Cred, Repo),
    Owner = maps:get(<<"owner">>, RepoInfo),
    #{<<"type">> := Type} = Owner,

    case Type of
        <<"User">> ->
            egithub:remove_collaborator(Cred, Repo, Username);
        _Org ->
            ok
    end.
