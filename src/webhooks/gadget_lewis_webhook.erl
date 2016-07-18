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
       , <<"repo">> := #{<<"ssh_url">> := GitUrl}
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
    ok = create_local_properties(RepoDir),
    ok = run_lewis(RepoDir),
    Comments = comments_from_lewis(RepoDir),
    Messages = messages_from_lewis(Comments, GithubFiles),

    {ok, Messages}
  catch
    _:{error, {status, ExitStatus, Output}} ->
      gadget_utils:catch_error_source( Output
                                     , ExitStatus
                                     , lewis
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

create_local_properties(RepoDir) ->
  SdkPath = application:get_env(gadget, sdk_path),
  os:putenv("ANDROID_HOME", SdkPath),
  AndroidSDK = os:getenv("ANDROID_HOME"),
  LocalPropPath = filename:join(RepoDir, "local.properties"),
  LocalPropData = ["sdk.dir=" , AndroidSDK],
  ok = file:write_file(LocalPropPath, io_lib:fwrite("~s\n", [LocalPropData])).

run_lewis(RepoDir) ->
  Command = ["cd ", RepoDir, "; ", "./gradlew lint --stacktrace"],
  _OutPut = gadget_utils:run_command(Command),
  XmlFilePath = lint_result_xml_path(RepoDir),
  LintResultExist = filelib:is_file(XmlFilePath),
  case LintResultExist of
    false   -> throw({error, {status, 1, "Not lint-results.xml file found"}});
    true    -> ok
  end.

comments_from_lewis(RepoDir) ->
  XmlFilePath = lint_result_xml_path(RepoDir),
  {XmlComments, _} = xmerl_scan:file(XmlFilePath),
  {issues, _, RawComments} = xmerl_lib:simplify_element(XmlComments),
  Comments = trim_separator_xmerl(RawComments),
  lists:flatmap(fun format_lewis_to_gadget_comment/1, Comments).

format_lewis_to_gadget_comment({issue, Tags, RawLocations}) ->
  Locations = trim_separator_xmerl(RawLocations),

  [{id, Id},
   {severity, Severity},
   {message, Msg},
   {category, Category},
   {priority, _PriorityNumber},
   {summary, _Summary},
   {explanation, Explanation}
   | _Rest
  ] = Tags,

  FunMapLocations =
   fun({location, LocationTagsData, _}) ->
     File = proplists:get_value(file, LocationTagsData, ""),
     Line = proplists:get_value(line, LocationTagsData, 0),
     #{ file   => File
      , number => list_to_integer(Line)
      , text   => format_text_messge(Id, Severity, Msg, Category, Explanation)
      }
   end,
  %% For one error/warning we can get several locations in reviewing the code.
  lists:map(FunMapLocations, Locations).

format_text_messge(Id, Severity, Msg, Category, Explanation) ->
  [ "\n"
  , "| Id                  | Severity | Category    |", "\n"
  , "|---------------------|----------|-------------|", "\n"
  , "| ", Id , " | ", Severity , " | " , Category,  " |", "\n"
  , " **Message**: ", Msg, ".\n "
  , " **Explanation**: ", Explanation, ".\n "
  ].

trim_separator_xmerl(XmlerData) ->
  %% The XmlerData had "\n        " separator between every tuple() with data.
  lists:filter(fun erlang:is_tuple/1, XmlerData).

lint_result_xml_path(RepoDir) ->
  filename:join(RepoDir, "app/build/outputs/lint-results-debug.xml").

messages_from_lewis(Comments, GithubFiles) ->
  gadget_utils:messages_from_comments("Lewis", Comments, GithubFiles).
