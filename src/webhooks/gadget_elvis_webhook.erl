%%% @doc Elvis webhook
-module(gadget_elvis_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%% @private
-spec handle_pull_request(
  egithub:credentials(), egithub_webhook:req_data(),
  [egithub_webhook:file()]) ->
  {ok, [egithub_webhook:message()]} | {error, term()}.
handle_pull_request(Cred, Req, GithubFiles) ->
  case elvis_webhook:handle_pull_request(Cred, Req, GithubFiles) of
    {ok, Messages} -> {ok, format_messages(Messages)};
    {error, Error} -> {error, Error}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

format_messages(Messages) -> lists:map(fun format_message/1, Messages).

format_message(Message) ->
  #{text := Text} = Message,
  FullText = gadget_utils:format_message("Elvis", Text),
  Message#{text => FullText}.
