%%% @doc Elvis webhook
-module(gadget_elvis_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

%% @todo: remove when issue #303 on elvis is fixed
-dialyzer([{no_return, [handle_pull_request/3]}]).
-dialyzer([{no_unused, [format_messages/1]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%% @private
-spec handle_pull_request(
  egithub:credentials(), egithub_webhook:req_data(),
  [egithub_webhook:file()]) ->
  {ok, [egithub_webhook:message()]}.
handle_pull_request(Cred, Req, GithubFiles) ->
  #{ <<"repository">> := Repository
   , <<"number">> := Number
   } = Req,
  #{ <<"full_name">> := RepoName
   } = Repository,
  try
    {ok, Messages} = elvis_webhook:handle_pull_request(Cred, Req, GithubFiles),
    {ok, format_messages(Messages)}
  catch
    _:Error ->
      Comments =
          [#{ file   => "elvis.config"
            , number => 0
            , text   => Error
            }],
      Messages1 =
        gadget_utils:messages_from_comments("Elvis",
                                            Comments,
                                            GithubFiles),
      gadget_utils:report_error( elvis
                               , Messages1
                               , RepoName
                               , 1
                               , lists:flatten(io_lib:format("~p", [Error]))
                               , Number)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

format_messages(Messages) -> lists:map(fun format_message/1, Messages).

format_message(Message) ->
  #{text := Text} = Message,
  FullText = gadget_utils:format_message("Elvis", Text),
  Message#{text => FullText}.
