%%% @doc Elvis webhook
-module(gadget_elvis_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3, handle_error/3]).

%% @todo: remove when issue #303 on elvis is fixed
-dialyzer([{no_return, [handle_pull_request/3]}]).
-dialyzer([{no_unused, [format_messages/1]}]).
-dialyzer([{no_match, [handle_pull_request/3]}]).

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
  #{<<"full_name">> := RepoName} = Repository,
  try
    {ok, Messages} = elvis_webhook:handle_pull_request(Cred, Req, GithubFiles),
    {ok, format_messages(Messages)}
  catch
    _:Error ->
      gadget_utils:catch_error_source( io_lib:format("~p", [Error])
                                     , 1
                                     , elvis
                                     , GithubFiles
                                     , RepoName
                                     , Number
                                     )
  end.

-spec handle_error( {error, term()}
                  , egithub_webhook:req_data()
                  , [egithub_webhook:file()]) ->
  {error, {failed, integer()}, string()} | {ok, [map()], string()}.
handle_error(Error, ReqData, GithubFiles) ->
  #{ <<"repository">> := Repository
   , <<"number">> := Number
   } = ReqData,
  #{<<"full_name">> := RepoName} = Repository,

  {Output, ExitStatus} =
    case Error of
      {badmatch, {error, {Status, Out, _}}} -> {Out, Status};
      {error, {status, Status, Out}} -> {Out, Status};
      FullErr -> {FullErr, 1}
    end,

  gadget_utils:catch_error_source( io_lib:format("~p", [Output])
                                 , ExitStatus
                                 , elvis
                                 , GithubFiles
                                 , RepoName
                                 , Number
                                 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

format_messages(Messages) -> lists:map(fun format_message/1, Messages).

format_message(Message) ->
  #{text := Text} = Message,
  FullText = gadget_utils:format_message("Elvis", Text),
  Message#{text => FullText}.
