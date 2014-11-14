-module(gadget_utils).

-export([
         hook_by_url/2,
         is_public_repo/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec hook_by_url(string(), [map()]) -> boolean().
hook_by_url(WebhookUrl, Hooks) ->
    Fun = fun
              (#{<<"config">> := #{<<"url">> := Url}})
                when Url == WebhookUrl ->
                  true;
              (_Other) ->
                  false
          end,
    case lists:filter(Fun, Hooks) of
        [] ->
            not_found;
        [Hook] -> {ok, Hook}
    end.

-spec is_public_repo(map()) -> boolean().
is_public_repo(#{<<"private">> := Private}) ->
    not Private.
