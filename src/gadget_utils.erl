-module(gadget_utils).

-export([ enabled_tools/2
        , tool_info/3
        , is_public/1
        , is_admin/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enabled_tools(map(), [map()]) -> [atom()].
enabled_tools(Tools, Hooks) ->
  [tool_info(ToolName, Tools, Hooks) || ToolName <- maps:keys(Tools)].

-spec tool_info(atom(), map(), [map()]) -> atom().
tool_info(ToolName, Tools, Hooks) ->
  ToolUrl = maps:get(ToolName, Tools),
  Fun =
    fun (#{<<"config">> := #{<<"url">> := HookUrl}}) ->
      list_to_binary(ToolUrl) == HookUrl;
        (_) -> false
    end,
  FilteredHooks = lists:filter(Fun, Hooks),
  Status =
    case FilteredHooks of
      [] -> off;
      _  -> on
    end,

  HookId =
    case Status of
      on ->
        #{<<"id">> := Id} = hd(FilteredHooks),
        Id;
      off -> undefined
    end,

  #{ name => ToolName
   , status => Status
   , hook_id => HookId}.

-spec is_public(map()) -> boolean().
is_public(#{<<"private">> := Private}) -> not Private.

-spec is_admin(map()) -> boolean().
is_admin(#{<<"permissions">> := #{<<"admin">> := true}}) -> true;
is_admin(_Repo) -> false.
