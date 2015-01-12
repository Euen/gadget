-module(gadget_compiler_webhook).

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

-spec handle_pull_request(
        egithub:credentials(), egithub_webhook:req_data(),
        [egithub_webhook:file()]) ->
        {ok, [egithub_webhook:messge()]} | {error, term()}.
handle_pull_request(Cred, ReqData, GithubFiles) ->
 #{ <<"repository">> := Repository
    , <<"pull_request">> := PR
    } = ReqData,
  #{  <<"name">>    := RepoName
    , <<"git_url">> := GitUrl
    } = Repository,
  #{ <<"head">> :=
      #{ <<"ref">> := Branch}
    } = PR,
  TmpRoot = application:get_env(gadget, tmp_path, "/tmp/gadget"),
  RepoDir = binary_to_list(filename:join(TmpRoot, RepoName)),

  try
    ensure_dir_deleted(RepoDir),
    clone_repo(GitUrl, RepoDir, Branch),
    Messages = compile_project(RepoDir),
    {ok, []}
  catch
    _:Error ->
      lager:warning(
        "Couldn't process PR: ~p~nParams: ~p~nStack: ~p",
        [Error, [Cred, ReqData, GithubFiles], erlang:get_stacktrace()]),
      {error, Error}
  after
    ensure_dir_deleted(RepoDir)
  end.

clone_repo(GitUrl, RepoDir, Branch) ->
  ensure_dir(RepoDir),
  Command =
    io_lib:format(
      "git clone -v -b ~s ~s ~s", [Branch, GitUrl, RepoDir]),
  Result = os:cmd(Command),
  lager:alert("~n~s", [Result]).

ensure_dir_deleted(RepoDir) -> os:cmd("rm -r " ++ RepoDir).

ensure_dir(RepoDir) ->
  case filelib:ensure_dir(RepoDir) of
    ok -> ok;
    {error, Error} -> throw(Error)
  end.

compile_project(RepoDir) ->
  Output =
    case filelib:is_regular(filename:join(RepoDir, "Makefile")) of
      true -> make_project(RepoDir);
      false ->
        case filelib:is_regular(filename:join(RepoDir, "rebar.config")) of
          true -> rebarize_project(RepoDir);
          false -> throw(cant_compile)
        end
    end,
  lager:alert("~n~s", [Output]),
  [].

make_project(RepoDir) ->
  os:cmd("cd " ++ RepoDir ++ "; V=1000 make").

rebarize_project(RepoDir) ->
  os:cmd("cd " ++ RepoDir ++ "; rebar --verbose get-deps compile").
