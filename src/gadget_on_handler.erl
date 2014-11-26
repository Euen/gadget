-module(gadget_on_handler).

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
    {ToolName, Req1} =  cowboy_req:binding(tool, Req),
    {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
    {Repo, _} = cowboy_req:qs_val(<<"repo">>, Req, ""),
    Cred = egithub:oauth(Token),

    Events = ["pull_request"],
    Headers = [{<<"content-type">>, <<"text/html">>}],
    Body = [],

    {ok, WebhookMap} = application:get_env(gadget, webhooks),
    ToolsList = lists:map(fun atom_to_list/1, maps:keys(WebhookMap)),
    ToolNameAtom = binary_to_atom(ToolName, utf8),  
    {ok, Req2} =
      case lists:member(atom_to_list(ToolNameAtom), ToolsList) of
        true -> 
            case ToolNameAtom of
              elvis -> gadget_elvis:on(Repo, Cred, Events, ToolNameAtom);
              _ -> cowboy_req:reply(404, Headers, Body, Req1)
            end,
            cowboy_req:reply(204, Headers, Body, Req1);
        false   -> 
            cowboy_req:reply(404, Headers, Body, Req1)
      end,
    {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.