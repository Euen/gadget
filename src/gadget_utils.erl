-module(gadget_utils).

-export([ enabled_tools/2
        , is_enabled/2
        , is_public_repo/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enabled_tools(map(), [map()]) -> [atom()].
enabled_tools(Tools, Hooks) ->
    [ToolName || ToolName <- maps:keys(Tools),
                 is_enabled(maps:get(ToolName, Tools), Hooks)].

-spec is_enabled(map(), [map()]) -> boolean().
is_enabled(#{url := ToolUrl}, Hooks) ->
    lists:any(
        fun(#{<<"config">> := #{<<"url">> := HookUrl}}) ->
            ToolUrl == binary_to_list(HookUrl)
        end, Hooks).

-spec is_public_repo(map()) -> boolean().
is_public_repo(#{<<"private">> := Private}) ->
    not Private.
