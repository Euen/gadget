-module(gadget_on_handler).

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         handle_post/2,
         terminate/3
        ]).

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
    {ok, Req, State} | {shutdown, Req, State}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), term()) -> 
  {ok, cowboy_req:req(), #state{}}.
rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

-spec allowed_methods(cowboy_req:req(), term()) -> 
  {[], cowboy_req:req(), term()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), term()) -> 
  {[], cowboy_req:req(), #state{}}.
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, handle_post}
   ],
   Req, State}.

-spec handle_post(cowboy_req:req(), #state{}) -> ok.
handle_post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    Decoded = jiffy:decode(Body, [return_maps]),
    ToolName = maps:get(<<"tool">>, Decoded),
    Repo = maps:get(<<"repo">>, Decoded),
    {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
    Cred = egithub:oauth(Token),

    Events = ["pull_request"],

    {ok, WebhookMap} = application:get_env(gadget, webhooks),
    ToolsList = lists:map(fun atom_to_list/1, maps:keys(WebhookMap)),
    ToolNameAtom = binary_to_atom(ToolName, utf8),
    {St} =
      case lists:member(atom_to_list(ToolNameAtom), ToolsList) of
        true -> 
            case ToolNameAtom of
              elvis -> 
              gadget_elvis:on(Repo, Cred, Events, ToolNameAtom);
              _ -> throw("Invalid tool.")
            end,
            {true};
        false ->
            {false}
      end,
    {St, Req1, State}.

-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.