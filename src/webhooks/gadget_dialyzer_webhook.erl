-module(gadget_dialyzer_webhook).

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
        "Couldn't clone project: ~p~nParams: ~p~nStack: ~p",
        [Error, [Cred, ReqData, GithubFiles], erlang:get_stacktrace()]),
      {error, Error}
  end.

process_pull_request(RepoDir, RepoName, Branch, GitUrl, GithubFiles) ->
  try
    ok = gadget_utils:clone_repo(RepoDir, Branch, GitUrl),
    gadget_utils:compile_project(RepoDir),
    Comments = dialyze_project(RepoDir),
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

dialyze_project(RepoDir) -> [].
