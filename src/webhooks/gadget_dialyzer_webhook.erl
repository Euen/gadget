%%% @doc Dialyzer webhook
-module(gadget_dialyzer_webhook).

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
    BuildTool = gadget_utils:build_tool_type(RepoDir),
    _ = gadget_utils:compile_project(RepoDir, silent),
    Comments =
      case BuildTool of
        makefile ->
          dialyze_make_project(RepoDir);
        rebar3   -> 
          dialyze_rebar3_project(RepoDir)
       end,
    Messages = messages_from_dialyzer(Comments, GithubFiles),
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

build_make_plt(VerbOption, RepoDir) ->
  PltCommand = "gadget-plt",
  Command = build_makefile_commands(RepoDir, VerbOption, PltCommand),
  gadget_utils:run_command(Command).

dialyze_make_project(RepoDir) ->
  VerbOption = gadget_utils:default_verbosity(makefile),
  build_make_plt(VerbOption, RepoDir),
  DialyzeCommand = "gadget-dialyze",
  Command = build_makefile_commands(RepoDir, VerbOption, DialyzeCommand),
  run_dialyze(RepoDir, Command). 

dialyze_rebar3_project(RepoDir) ->
  Command = build_rebar3_commands(RepoDir),
  run_dialyze(RepoDir, Command). 

run_dialyze(RepoDir, Command) ->
  ResultFile = filename:join(RepoDir, "gadget-dialyze.result"),
  remove_result_file(ResultFile),
  Output =
    try
      gadget_utils:run_command(Command)
    catch
      % Since rebar3 adds an ugly warning counter at the end of the warnings,
      % it makes the command execution to exit with `ExitStatus=1', so we
      % ignore that exception and return it as the output for the last command.
      _:{error, {status, 1, []}} = Error ->
        Error;
      _:Error ->
        % If it doesn't fit the match before, it's not the exception we are
        % looking for, so we re-throw it.
        throw(Error)
    end,
  case filelib:is_regular(ResultFile) of
    false -> throw({error, Output});
    true ->
      case file:consult(ResultFile) of
        {ok, Results} ->
          [generate_comment(RepoDir, Result) || Result <- Results];
        {error, _Error} -> % parse error: the text is the error description
          {ok, FileContents} = file:read_file(ResultFile),
          case is_rebar3_output(FileContents) of
            true ->
              [File | Warnings] = string:tokens(binary_to_list(FileContents), "\n"),
              lager:critical("File -> ~p", [File]),
              lager:critical("Warnings -> ~p", [Warnings]),
              Warnings;
            false ->
              [#{file => <<>>, number => 0, text => FileContents}]
          end
      end
  end.

generate_comment(RepoDir, Warning = {_, {Filename, Line}, _}) ->
  #{ file   => re:replace(Filename, [$^ | RepoDir], "", [{return, binary}])
   , number => Line
   , text   => generate_comment_text(Warning)
   }.

generate_comment_text(Warning) ->
  FromDialyzer = dialyzer:format_warning(Warning, basename),
  [_File, _Line | MessageParts] = string:tokens(FromDialyzer, [$:]),
  string:join(MessageParts, ":").

priv_dir() ->
  case code:priv_dir(gadget) of
    {error, bad_name} -> "priv";
    Dir -> Dir
  end.

build_rebar3_commands(RepoDir) ->
% `TERM=dumb' means that our shell doesn't have colors capability.
% For more info about `TERM' please check
% here https://en.wikipedia.org/wiki/Termcap .
% Here is what `rebar3' uses to check color capability
% https://github.com/project-fifo/cf/blob/master/src/cf_term.erl
  [ "cd "
  , RepoDir
  , "; TERM=dumb QUIET=1 "
  , "rebar3 "
  , "dialyzer > gadget_dialyze.result"].


remove_result_file(ResultFile) ->
  case filelib:is_regular(ResultFile) of
    true -> file:delete(ResultFile);
    false -> ok
  end.

is_rebar3_output(FileContents) ->
  ContentList = string:tokens(binary_to_list(FileContents), "\n"),
  LastLine = lists:last(ContentList),
  case re:run(LastLine, ".* Warnings occured running dialyzer*") of
    nomatch -> false;
    {match, _Captured} -> true
  end.

build_makefile_commands(RepoDir, VerbOpt, Rule) ->
  GadgetMk = filename:absname(filename:join(priv_dir(), "gadget.mk")),
  ["cd ", RepoDir, "; ", VerbOpt, "make -f ", GadgetMk, " ", Rule].

messages_from_dialyzer(Comments, GithubFiles) ->
  gadget_utils:messages_from_comments("Dialyzer", Comments, GithubFiles).
