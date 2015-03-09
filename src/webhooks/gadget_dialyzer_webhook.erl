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
    case filelib:is_regular(filename:join(RepoDir, "erlang.mk")) of
      false -> {error, "Only erlang.mk based repos can be dialyzed"};
      true ->
        gadget_utils:compile_project(RepoDir),
        build_plt(RepoDir),
        Comments = dialyze_project(RepoDir),
        {ok, gadget_utils:messages_from_comments(Comments, GithubFiles)}
    end
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

build_plt(RepoDir) ->
  gadget_utils:run_command(["cd ", RepoDir, "; V=1000 make plt"]).

dialyze_project(RepoDir) ->
  GadgetMk =
    filename:absname(filename:join(priv_dir(), "gadget.mk")),
  Command = ["cd ", RepoDir, "; V=1000 make -f ", GadgetMk, " gadget-dialyze"],
  Output = gadget_utils:run_command(Command),
  ResultFile = filename:join(RepoDir, "gadget-dialyze.result"),
  case filelib:is_regular(ResultFile) of
    false -> throw({error, Output});
    true ->
      {ok, Results} = file:consult(ResultFile),
      [generate_comment(RepoDir, Result) || Result <- Results]
  end.

generate_comment(RepoDir, Warning = {_, {Filename, Line}, _}) ->
  #{ file   => re:replace(Filename, [$^ | RepoDir], "", [{return, binary}])
   , number => Line
   , text   => generate_comment_text(Warning)
   }.

generate_comment_text(Warning) ->
  FromDialyzer = dialyzer:format_warning(Warning),
  [_File, _Line | MessageParts] = string:tokens(FromDialyzer, [$:]),
  Message = string:join(MessageParts, ":"),
  iolist_to_binary(["According to **Dialyzer**:\n> ", Message]).

priv_dir() ->
  case code:priv_dir(gadget) of
    {error, bad_name} -> "priv";
    Dir -> Dir
  end.
