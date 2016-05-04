%%% @doc Dialyzer webhook
-module(gadget_lewis_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

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
  #{ <<"full_name">> := RepoName
   } = Repository,
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

process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles, Number) ->
  try
    ok = gadget_utils:clone_repo(RepoDir, Branch, GitUrl),
    _ = ktn_os:command("./gradlew lint"), 
    XmlCommentsPath = "app/build/outputs/lint-results-debug.xml",
    {XmlComments, _} = xmerl_scan:file(XmlCommentsPath),
    {issues, _, Comments} = xmerl_lib:simplify_element(XmlComments),
    Messages = messages_from_lewis(Comments, GithubFiles),
    {ok, Messages}
  catch
    _:{error, {status, ExitStatus, Output}} ->
      gadget_utils:catch_error_source( Output
                                     , ExitStatus
                                     , dialyzer
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

map_error({issue, Values, Location}) ->
  [{id, _},
   {severity, Error},
   {message, _Message},
   {category, _category},
   {priority, _PriorityNumber},
   {summary, _Summary},
   {explanation, _Explanation}
   | _Rest
  ] = Values,
  [_ , {location,[{file, File}, {line, Line}, {column, Column}]}, _] = Location,
  #{ file   => File
   , number => Line
   , text   => Error
   }.

messages_from_lewis(Comments, GithubFiles) ->
  gadget_utils:messages_from_comments("Lewis", Comments, GithubFiles).
