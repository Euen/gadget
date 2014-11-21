-module(gadget_utils).

-export([ enabled_tools/2
        , status/3
        , is_public_repo/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enabled_tools(map(), [map()]) -> [atom()].
enabled_tools(Tools, Hooks) ->
    [[ {name, ToolName}
     , {status, status(ToolName, Tools, Hooks)}]
     || ToolName <- maps:keys(Tools)].

-spec status(atom(), map(), [map()]) -> atom().
status(ToolName, Tools, Hooks) ->
    #{url := ToolUrl} = maps:get(ToolName, Tools),
    Fun =
        fun (#{<<"config">> := #{<<"url">> := HookUrl}}) ->
            list_to_binary(ToolUrl) == HookUrl
        end,
    case lists:filter(Fun, Hooks) of
        [] -> off;
        _  -> on
    end.

-spec is_public_repo(map()) -> boolean().
is_public_repo(#{<<"private">> := Private}) ->
    not Private.
