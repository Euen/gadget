%%% @doc Compiler webhook
-module(gadget_compiler_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3, handle_error/3]).

%% @private
-spec handle_pull_request(
        egithub:credentials(), egithub_webhook:req_data(),
        [egithub_webhook:file()]) ->
        {ok, [egithub_webhook:message()]} | {error, term()}.
handle_pull_request(Cred, ReqData, GithubFiles) ->
  #{ <<"repository">> := Repository
   , <<"pull_request">> := PR
   , <<"number">> := Number
   } = ReqData,
  #{<<"full_name">> := RepoName} = Repository,
  #{ <<"head">> :=
      #{ <<"ref">> := Branch
       , <<"repo">> := #{<<"clone_url">> := GitUrl}
       }
   } = PR,

  try gadget_utils:ensure_repo_dir(RepoName) of
    RepoDir ->
      process_pull_request( RepoDir
                          , RepoName
                          , Branch
                          , GitUrl
                          , GithubFiles
                          , Number
                          )
  catch
    _:Error ->
      _ = lager:warning(
        "Couldn't clone project: ~p~nParams: ~p~nStack: ~p",
        [Error, [Cred, ReqData, GithubFiles], erlang:get_stacktrace()]),
      {error, Error}
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
                                 , compiler
                                 , GithubFiles
                                 , RepoName
                                 , Number
                                 ).

process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles, Number) ->
  try
    ok = gadget_utils:clone_repo(RepoDir, Branch, GitUrl),
    Comments = gadget_utils:extract_errors(
                 gadget_utils:compile_project(RepoDir, verbose)),
    Messages =
      gadget_utils:messages_from_comments("Compiler", Comments, GithubFiles),
    {ok, Messages}
  catch
    _:{error, {status, ExitStatus, Output}} ->
      gadget_utils:catch_error_source( Output
                                     , ExitStatus
                                     , compiler
                                     , GithubFiles
                                     , RepoName
                                     , Number
                                     );
    _:Error ->
      _ = lager:warning(
        "Couldn't process PR: ~p~nParams: ~p~nStack: ~p",
        [ Error
        , [RepoDir, RepoName, Branch, GitUrl, GithubFiles]
        , erlang:get_stacktrace()
        ]),
      {error, Error}
  after
    gadget_utils:ensure_dir_deleted(RepoDir)
  end.
