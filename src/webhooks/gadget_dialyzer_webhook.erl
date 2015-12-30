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
    case filelib:is_regular(filename:join(RepoDir, "erlang.mk")) of
      false -> {error, "Only erlang.mk based repos can be dialyzed"};
      true ->
        gadget_utils:compile_project(RepoDir, silent),
        build_plt(RepoDir),
        Comments = dialyze_project(RepoDir),
        Messages =
          gadget_utils:messages_from_comments(
            "Dialyzer", Comments, GithubFiles),
        {ok, Messages}
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
  FromDialyzer = dialyzer:format_warning(Warning),
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
