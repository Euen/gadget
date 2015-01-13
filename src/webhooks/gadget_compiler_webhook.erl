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
  #{  <<"full_name">> := RepoName
    , <<"git_url">> := GitUrl
    } = Repository,
  #{ <<"head">> :=
      #{ <<"ref">> := Branch}
    } = PR,
  TmpRoot = application:get_env(gadget, tmp_path, "/tmp/gadget"),
  Now = unique_id(),
  RepoDir = binary_to_list(filename:join([TmpRoot, RepoName, Now])),

  try
    ensure_dir_deleted(RepoDir),
    clone_repo(GitUrl, RepoDir, Branch),
    Comments = compile_project(RepoDir),
    {ok, messages_from_comments(Comments, GithubFiles)}
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
  run_command(Command).

ensure_dir_deleted(RepoDir) -> run_command("rm -r " ++ RepoDir).

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
  Lines = re:split(Output, "\n", [{return, binary}, trim]),
  extract_errors(Lines).

make_project(RepoDir) ->
  run_command("cd " ++ RepoDir ++ "; V=1000 make").

rebarize_project(RepoDir) ->
  run_command("cd " ++ RepoDir ++ "; rebar --verbose get-deps compile"),
  run_command("cd " ++ RepoDir ++ "; rebar skip_deps=true clean compile").

run_command(Command) ->
  lager:info("~s", [Command]),
  Result = os:cmd(Command),
  HR = [$~ || _ <- lists:seq(1, 80)],
  lager:debug("~n~s~n$ ~s~n~s~n~s", [HR, Command, Result, HR]),
  Result.

extract_errors(Lines) ->
  {ok, Regex} = re:compile(<<"(.+):([0-9]*): (.+)">>),
  extract_errors(Lines, Regex, []).
extract_errors([], _Regex, Errors) -> Errors;
extract_errors([Line|Lines], Regex, Errors) ->
  NewErrors =
    case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
      {match, [File, Number, Comment]} ->
        [#{ file   => File
          , number => binary_to_integer(Number)
          , text   => Comment
          } | Errors];
      {match, Something} -> lager:emergency("~p", [Something]);
      _ ->
        Errors
    end,
  extract_errors(Lines, Regex, NewErrors).

messages_from_comments(Comments, GithubFiles) ->
  lists:flatmap(
    fun(Comment) ->
      messages_from_comment(Comment, GithubFiles)
    end, Comments).

messages_from_comment(Comment, GithubFiles) ->
  #{ file   := File
   , number := Line
   , text   := Text
   } = Comment,
  MatchingFiles =
    [GithubFile
    || #{ <<"filename">>  := Filename
        , <<"status">>    := Status
        } = GithubFile <- GithubFiles
      , Filename == File
      , Status /= <<"deleted">>
    ],
  case MatchingFiles of
    [] -> [];
    [MatchingFile|_] ->
      messages_from_comment(File, Line, Text, MatchingFile)
  end.

messages_from_comment(Filename, Line, Text, File) ->
  #{ <<"patch">>      := Patch
   , <<"raw_url">>    := RawUrl
   } = File,
  case elvis_git:relative_position(Patch, Line) of
    {ok, Position} ->
      [ #{commit_id => commit_id_from_raw_url(RawUrl, Filename),
          path      => Filename,
          position  => Position,
          text      => Text
          }
      ];
    not_found ->
      lager:info("Line ~p does not belong to file's diff.", [Line]),
      []
  end.

%% @doc Gets a raw_url for a file and extracts the commit id from it.
-spec commit_id_from_raw_url(binary(), binary()) -> string().
commit_id_from_raw_url(Url, Filename) ->
  Regex = <<".+/raw/(.+)/", Filename/binary>>,
  {match, [CommitId]} = re:run(Url, Regex, [{capture, all_but_first, binary}]),
  binary_to_list(CommitId).

unique_id() ->
  {X, Y, Z} = os:timestamp(),
  iolist_to_binary(io_lib:format("~7.10.0B-~7.10.0B-~7.10.0B", [X, Y, Z])).
