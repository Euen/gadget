%%% @doc POST /off handler
-module(gadget_off_handler).

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , delete_resource/2
        , terminate/3
        ]).

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init(term(), cowboy_req:req(), term()) ->
  {term(), term(), term()}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @private
-spec rest_init(cowboy_req:req(), term()) ->
  {ok, cowboy_req:req(), #state{}}.
rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

%% @private
-spec allowed_methods(cowboy_req:req(), term()) ->
  {[_], cowboy_req:req(), term()}.
allowed_methods(Req, State) ->
  {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

%% @private
-spec delete_resource(cowboy_req:req(), #state{}) -> ok.
delete_resource(Req, State) ->
  {ToolNameBin, _} =  cowboy_req:binding(tool, Req),
  {Token, _} = cowboy_req:cookie(<<"token">>, Req, ""),
  {Repo, _} = cowboy_req:qs_val(<<"repo">>, Req, ""),
  %Cred = egithub:oauth(Token),

  Tool = binary_to_atom(ToolNameBin, utf8),
  %%%%
% {ok, Hooks} = egithub:hooks(Cred, Repo),
% EnabledTools = gadget_utils:active_tools(Hooks),
% HIds =
%   [HookId
%    || #{ hook_id := HookId
%        , name    := ToolName
%        , status  := on
%        } <- EnabledTools
%    , ToolName == Tool
%    ],
% ok =
%   case HIds of
%     [] -> ok;
%     [Id] -> egithub:delete_webhook(Cred, Repo, Id)
%   end,
  gadget:unregister(Repo, Tool,Token),
  {true, Req, State}.

%% @private
-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) -> ok.
