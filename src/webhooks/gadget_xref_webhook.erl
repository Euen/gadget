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
  #{<<"full_name">> := RepoName} = Repository,
  #{ <<"head">> :=
      #{ <<"ref">> := Branch
       , <<"repo">> := #{<<"clone_url">> := GitUrl}
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
    Comments =
      case filelib:is_regular(filename:join(RepoDir, "rebar.config")) of
        true -> xref_project(RepoDir);
        false ->
          case filelib:is_regular(filename:join(RepoDir, "erlang.mk")) of
            false ->
              throw({error,
                          {status, 1, "Not rebar.config nor erlang.mk found"}});
            true ->
              xref_project(RepoDir)
          end
      end,
    Messages =
      gadget_utils:messages_from_comments("Xref", Comments, GithubFiles),
    {ok, Messages}
  catch
    _:{error, {status, ExitStatus, Output}} ->
      gadget_utils:catch_error_source( Output
                                     , ExitStatus
                                     , xref
                                     , GithubFiles
                                     , RepoName
                                     , Number
                                     );
    _:Error ->
      _ = lager:warning(
        "Couldn't process PR: ~p~nParams: ~p~nStack: ~p",
        [ Error
        , [RepoDir, RepoName, Branch, GitUrl, GithubFiles, Number]
        , erlang:get_stacktrace()
        ]),
      {error, Error}
  after
    gadget_utils:ensure_dir_deleted(RepoDir)
  end.

xref_project(RepoDir) ->
  RepoNode = start_node(RepoDir),
  try
    ok = set_cwd(RepoNode, RepoDir),
    XrefWarnings = run_xref(RepoNode),
    [generate_comment(RepoDir, XrefWarning) || XrefWarning <- XrefWarnings]
  catch
    _:Error ->
      _ = lager:warning(
        "Couldn't process PR: ~p~nParams: ~p~nStack: ~p",
        [ Error
        , [RepoDir]
        , erlang:get_stacktrace()
        ]),
      Stacktrace = ktn_debug:ppst(erlang:get_stacktrace(),
      throw({error, {status, 1, Stacktrace}})
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
