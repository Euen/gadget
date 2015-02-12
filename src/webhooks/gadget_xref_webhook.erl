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
  XrefResult = run_xref(RepoNode),
  generate_comments(XrefResult).

start_node(RepoDir) ->
  UniqueId = filename:basename(RepoDir),
  NodeName = list_to_atom(UniqueId),
  [_, HostBin] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
  Host = binary_to_atom(HostBin, 2),
  slave:start_link(Host, NodeName).

set_cwd(RepoNode, RepoDir) ->
  rpc:call(RepoNode, file, set_cwd, [RepoDir]).

run_xref(RepoNode) ->
  RunnerEbin = filename:dirname(code:which(xref_runner)),
  true = rpc:call(RepoNode, code, add_path, [RunnerEbin]),
  rpc:call(RepoNode, xref_runner, check, []).

