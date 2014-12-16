-module(gadget_utils).

-export([ enabled_tools/2
        , tool_info/3
        , is_public_repo/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enabled_tools(map(), [map()]) -> [atom()].
enabled_tools(Tools, Hooks) ->
    [tool_info(ToolName, Tools, Hooks)
     || ToolName <- maps:keys(Tools)].

-spec tool_info(atom(), map(), [map()]) -> atom().
tool_info(ToolName, Tools, Hooks) ->
    ToolUrl = maps:get(ToolName, Tools),
    Fun =
        fun (#{<<"config">> := #{<<"url">> := HookUrl}}) ->
            list_to_binary(ToolUrl) == HookUrl
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

    #{name => ToolName,
      status => Status,
      hook_id => HookId}.

-spec is_public_repo(map()) -> boolean().
is_public_repo(#{<<"private">> := Private}) ->
    not Private.
