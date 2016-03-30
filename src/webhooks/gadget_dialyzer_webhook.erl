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
      process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles, Number)
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
    case filelib:is_regular(filename:join(RepoDir, "erlang.mk")) of
      false -> {error, "Only erlang.mk based repos can be dialyzed"};
      true ->
        _ = gadget_utils:compile_project(RepoDir, silent),
        _ = build_plt(RepoDir),
        Comments = dialyze_project(RepoDir),
        Messages =
          gadget_utils:messages_from_comments(
            "Dialyzer", Comments, GithubFiles),
        {ok, Messages}
    end
  catch
    _:{error, {status, ExitStatus, Output}} ->
        Lines = gadget_utils:output_to_lines(Output),
        case error_source(Lines) of
          unknown -> {error, {failed, ExitStatus}};
          dialyzer ->
            Comments1 = extract_errors(Lines),
            Messages1 =
              gadget_utils:messages_from_comments("dialyzer",
                                                  Comments1,
                                                  GithubFiles),
            gadget_utils:report_error( dialyzer
                                     , Messages1
                                     , RepoName
                                     , ExitStatus
                                     , Output
                                     , Number)
        end;
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
      {match, Something} ->
        _ = lager:error("WHAT? ~p", [Something]),
        [];
      _ ->
        Errors
    end,
  extract_errors(Lines, Regex, NewErrors).

-spec error_source([binary()]) -> dialyzer | unknown.
error_source(Lines) ->
  LastLines = lists:sublist(lists:reverse(Lines), 3),
  Regexes = ["make.*?[:] [*][*][*] [[][^]]*[]] Error",
             "ERROR[:] compile failed",
             "Compiling .* failed$"],
  MatchesRegexes =
    fun(Line) ->
      lists:any(fun(Regex) -> nomatch /= re:run(Line, Regex) end, Regexes)
    end,
  case lists:any(MatchesRegexes, LastLines) of
    true -> dialyzer;
    false -> unknown
  end.

build_plt(RepoDir) ->
  GadgetMk = filename:absname(filename:join(priv_dir(), "gadget.mk")),
  VerbOption = default_verbosity(),
  Command =
    ["cd ", RepoDir, "; ", VerbOption, "make -f ", GadgetMk, " gadget-plt"],
  gadget_utils:run_command(Command).

dialyze_project(RepoDir) ->
  GadgetMk =
    filename:absname(filename:join(priv_dir(), "gadget.mk")),
  VerbOption = default_verbosity(),
  Command =
    ["cd ", RepoDir, "; ", VerbOption, "make -f ", GadgetMk, " gadget-dialyze"],
  Output = gadget_utils:run_command(Command),
  ResultFile = filename:join(RepoDir, "gadget-dialyze.result"),
  case filelib:is_regular(ResultFile) of
    false -> throw({error, Output});
    true ->
      case file:consult(ResultFile) of
        {ok, Results} ->
          [generate_comment(RepoDir, Result) || Result <- Results];
        {error, _Error} -> % parse error: the text is the error description
          {ok, FileContents} = file:read_file(ResultFile),
          [#{file => <<>>, number => 0, text => FileContents}]
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

default_verbosity() ->
  case application:get_env(gadget, default_verbosity, silent) of
    verbose -> "V=2 ";
    silent -> ""
  end.