%%% @doc Xref Webhook
-module(gadget_xref_webhook).

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
      process_pull_request( RepoDir, RepoName, Branch, GitUrl, GithubFiles
                          , Number)
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
    _ = gadget_utils:compile_project(RepoDir, silent),
    Comments = xref_project(RepoDir),
    {ok, gadget_utils:messages_from_comments("Xref", Comments, GithubFiles)}
  catch
    _:{error, {status, ExitStatus, Output}} ->
        Lines = gadget_utils:output_to_lines(Output),
        case error_source(Lines) of
          unknown -> {error, {failed, ExitStatus}};
          xref ->
            Comments1 = extract_errors(Lines),
            Messages1 =
              gadget_utils:messages_from_comments("Xref",
                                                  Comments1,
                                                  GithubFiles),
            gadget_utils:report_error( xref
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



-spec error_source([binary()]) -> xref | unknown.
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
    true -> xref;
    false -> unknown
  end.

xref_project(RepoDir) ->
  RepoNode = start_node(RepoDir),
  try
    ok = set_cwd(RepoNode, RepoDir),
    XrefWarnings = run_xref(RepoNode),
    [generate_comment(RepoDir, XrefWarning) || XrefWarning <- XrefWarnings]
  after
    stop_node(RepoNode)
  end.

stop_node(RepoNode) ->
  gadget_slave:stop(RepoNode).

start_node(RepoDir) ->
  UniqueId = filename:basename(RepoDir),
  NodeName =
    case binary:split(atom_to_binary(node(), utf8), <<"@">>) of
      [_, HostName] ->
        binary_to_atom(iolist_to_binary([UniqueId, $@, HostName]), utf8);
      _JustNodeName ->
        list_to_atom(UniqueId)
    end,
  {ok, _} = gadget_slave:start(NodeName),
  NodeName.

set_cwd(RepoNode, RepoDir) ->
  rpc:call(RepoNode, file, set_cwd, [RepoDir]).

run_xref(RepoNode) ->
  RunnerEbin = filename:absname(filename:dirname(code:which(xref_runner))),
  Deps = rpc:call(RepoNode, filelib, wildcard, ["deps/*/ebin"]),
  ok = rpc:call(RepoNode, code, add_pathsa, [[RunnerEbin | Deps]]),
  rpc:call(RepoNode, xref_runner, check, []).

generate_comment(RepoDir, XrefWarning) ->
  #{ filename := Filename
   , line     := Line
   , source   := Source
   , check    := Check
   } = XrefWarning,
  Target = maps:get(target, XrefWarning, undefined),
  #{ file   => re:replace(Filename, [$^ | RepoDir], "", [{return, binary}])
   , number => Line
   , text   => generate_comment_text(Check, Source, Target)
   }.

generate_comment_text(Check, {SM, SF, SA}, TMFA) ->
  SMFA = io_lib:format("`~p:~p/~p`", [SM, SF, SA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(Check, SMFA, {TM, TF, TA}) ->
  TMFA = io_lib:format("`~p:~p/~p`", [TM, TF, TA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(undefined_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls undefined function ~s", [SMFA, TMFA]);
generate_comment_text(undefined_functions, SMFA, _TMFA) ->
  io_lib:format("~s is not defined as a function", [SMFA]);
generate_comment_text(locals_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is an unused local function", [SMFA]);
generate_comment_text(exports_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is an unused export", [SMFA]);
generate_comment_text(deprecated_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls deprecated function ~s", [SMFA, TMFA]);
generate_comment_text(deprecated_functions, SMFA, _TMFA) ->
  io_lib:format("~s is deprecated", [SMFA]).