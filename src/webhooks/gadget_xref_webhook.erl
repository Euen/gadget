-module(gadget_xref_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

-spec handle_pull_request(
        egithub:credentials(), egithub_webhook:req_data(),
        [egithub_webhook:file()]) ->
        {ok, [egithub_webhook:message()]} | {error, term()}.
handle_pull_request(Cred, ReqData, GithubFiles) ->
  #{  <<"repository">> := Repository
    , <<"pull_request">> := PR
    } = ReqData,
  #{  <<"full_name">> := RepoName
    , <<"git_url">> := GitUrl
    } = Repository,
  #{ <<"head">> :=
      #{ <<"ref">> := Branch}
    } = PR,

  try gadget_utils:ensure_repo_dir(RepoName) of
    RepoDir ->
      process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles)
  catch
    _:Error ->
      lager:warning(
        "Couldn't cclone project: ~p~nParams: ~p~nStack: ~p",
        [Error, [Cred, ReqData, GithubFiles], erlang:get_stacktrace()]),
      {error, Error}
  end.

process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles) ->
  try
    ok = gadget_utils:clone_repo(RepoDir, Branch, GitUrl),
    gadget_utils:compile_project(RepoDir),
    Comments = xref_project(RepoDir),
    {ok, gadget_utils:messages_from_comments(Comments, GithubFiles)}
  catch
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

xref_project(RepoDir) ->
  {ok, RepoNode} = start_node(RepoDir),
  ok = set_cwd(RepoNode, RepoDir),
  XrefWarnings = run_xref(RepoNode),
  stop_node(RepoNode),
  [generate_comment(RepoDir, XrefWarning) || XrefWarning <- XrefWarnings].

stop_node(RepoNode) ->
  slave:stop(RepoNode).

start_node(RepoDir) ->
  UniqueId = filename:basename(RepoDir),
  NodeName = list_to_atom(UniqueId),
  [_, HostBin] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
  Host = binary_to_atom(HostBin, utf8),
  slave:start_link(Host, NodeName).

set_cwd(RepoNode, RepoDir) ->
  rpc:call(RepoNode, file, set_cwd, [RepoDir]).

run_xref(RepoNode) ->
  RunnerEbin = filename:dirname(code:which(xref_runner)),
  true = rpc:call(RepoNode, code, add_path, [RunnerEbin]),
  rpc:call(RepoNode, xref_runner, check, []).

generate_comment(RepoDir, XrefWarning) ->
  #{ filename := Filename
   , line     := Line
   , source   := Source
   , check    := Check
   } = XrefWarning,
  Target = maps:get(target, XrefWarning, undefined),
  #{ file   => re:replace(Filename, [$^ | RepoDir], "", [{return, list}])
   , number => Line
   , text   => iolist_to_binary(generate_comment_text(Check, Source, Target))
   }.

generate_comment_text(Check, {SM, SF, SA}, TMFA) ->
  SMFA = io_lib:format("~p:~p/~p", [SM, SF, SA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(Check, SMFA, {TM, TF, TA}) ->
  TMFA = io_lib:format("~p:~p/~p", [TM, TF, TA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(undefined_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls undefined function ~s", [SMFA, TMFA]);
generate_comment_text(undefined_functions, SMFA, _TMFA) ->
  io_lib:format("~s is undefined function", [SMFA]);
generate_comment_text(locals_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is unused local function", [SMFA]);
generate_comment_text(exports_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is unused export", [SMFA]);
generate_comment_text(deprecated_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls deprecated function ~s", [SMFA, TMFA]);
generate_comment_text(deprecated_functions, SMFA, _TMFA) ->
  io_lib:format("~s is deprecated function", [SMFA]).
