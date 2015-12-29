%%% @doc Compiler webhook
-module(gadget_compiler_webhook).

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
      process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles)
  catch
    _:Error ->
      lager:warning(
        "Couldn't clone project: ~p~nParams: ~p~nStack: ~p",
        [Error, [Cred, ReqData, GithubFiles], erlang:get_stacktrace()]),
      {error, Error}
  end.

process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles) ->
  try
    ok = gadget_utils:clone_repo(RepoDir, Branch, GitUrl),
    Comments = extract_errors(gadget_utils:compile_project(RepoDir)),
    Messages =
      gadget_utils:messages_from_comments("Compiler", Comments, GithubFiles),
    {ok, Messages}
  catch
    _:{error, {status, ExitStatus, Output}} ->
      Lines = gadget_utils:output_to_lines(Output),
      case error_source (Lines) of
        unknown -> {error, {failed, ExitStatus}};
        compiler ->
          Comments1 = extract_errors(Lines),
          Messages1 =
            gadget_utils:messages_from_comments("Compiler",
                                                Comments1,
                                                GithubFiles),
          report_compiler_error(Messages1, ExitStatus)
      end;
    _:Error ->
      lager:warning(
        "Couldn't process PR: ~p~nParams: ~p~nStack: ~p",
        [ Error
        , [RepoDir, RepoName, Branch, GitUrl, GithubFiles]
        , erlang:get_stacktrace()
        ]),
      {error, Error}
  after
    gadget_utils:ensure_dir_deleted(RepoDir)
  end.

extract_errors(Lines) ->
  {ok, Regex} = re:compile(<<"(.+):([0-9]*): (.+)">>),
  extract_errors(Lines, Regex, []).
extract_errors([], _Regex, Errors) -> Errors;
extract_errors([Line|Lines], Regex, Errors) ->
  NewErrors =
    case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
      {match, [File, <<>>, Comment]} ->
        [#{ file   => File
          , number => 0
          , text   => Comment
          } | Errors];
      {match, [File, Number, Comment]} ->
        [#{ file   => File
          , number => binary_to_integer(Number)
          , text   => Comment
          } | Errors];
      {match, Something} -> lager:error("WHAT? ~p", [Something]);
      _ ->
        Errors
    end,
  extract_errors(Lines, Regex, NewErrors).

error_source(Lines) ->
  LastLines = lists:sublist(lists:reverse(Lines), 3),
  Regexes = ["make[:] [*][*][*] [[][^]]*[]] Error",
             "ERROR[:] compile failed",
             "Compiling .* failed$"],
  MatchesRegexes =
    fun(Line) ->
      lists:any(fun(Regex) -> nomatch /= re:run(Line, Regex) end, Regexes)
    end,
  case lists:any(MatchesRegexes, LastLines) of
    true -> compiler;
    false -> unknown
  end.

report_compiler_error([], ExitStatus) -> {error, {failed, ExitStatus}};
report_compiler_error([#{commit_id := CommitId} | _] = Messages, ExitStatus) ->
  ExtraMessage =
    #{commit_id => CommitId,
      path      => "",
      position  => 0,
      text      => <<"**Compiler** failed with exit status: ",
                     (integer_to_binary(ExitStatus))/binary>>
     },
  {ok, [ExtraMessage | Messages]}.
