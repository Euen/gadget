-module(gadget_utils).

-export([ active_tools/1
        , tool_info/3
        , is_public/1
        , is_admin/1
        , ensure_repo_dir/1
        , clone_repo/3
        , ensure_dir_deleted/1
        , run_command/1
        , unique_id/0
        , compile_project/1
        , messages_from_comments/3
        , format_message/2
        ]).

-type comment() :: #{file   => string(),
                     number => pos_integer(),
                     text   => binary()
                    }.
-export_type([comment/0]).

-spec active_tools([map()]) -> [atom()].
active_tools(Hooks) ->
  Tools = application:get_env(gadget, webhooks, #{}),
  [tool_info(ToolName, Tools, Hooks) || ToolName <- maps:keys(Tools)].

-spec tool_info(atom(), map(), [map()]) -> atom().
tool_info(ToolName, Tools, Hooks) ->
  ToolUrl = maps:get(ToolName, Tools),
  Fun =
    fun (#{<<"config">> := #{<<"url">> := HookUrl}}) ->
      list_to_binary(ToolUrl) == HookUrl;
        (_) -> false
    end,
  FilteredHooks = lists:filter(Fun, Hooks),
  Status =
    case FilteredHooks of
      [] -> off;
      _  -> on
    end,

  HookId =
    case Status of
      on ->
        #{<<"id">> := Id} = hd(FilteredHooks),
        Id;
      off -> undefined
    end,

  #{ name => ToolName
   , status => Status
   , hook_id => HookId}.

-spec is_public(map()) -> boolean().
is_public(#{<<"private">> := Private}) -> not Private.

-spec is_admin(map()) -> boolean().
is_admin(#{<<"permissions">> := #{<<"admin">> := true}}) -> true;
is_admin(_Repo) -> false.

-spec ensure_repo_dir(binary()) -> file:name_all().
ensure_repo_dir(RepoName) ->
  TmpRoot = application:get_env(gadget, tmp_path, "/tmp/gadget"),
  Now = unique_id(),
  RepoDir = binary_to_list(filename:join([TmpRoot, RepoName, Now])),
  ensure_dir_deleted(RepoDir),
  ensure_dir(RepoDir),
  RepoDir.

-spec clone_repo(file:name_all(), binary(), binary()) -> ok.
clone_repo(RepoDir, Branch, GitUrl) ->
  Command =
    io_lib:format(
      "git clone -v -b ~s ~s ~s", [Branch, GitUrl, RepoDir]),
  run_command(Command),
  ok.

-spec ensure_dir_deleted(file:name_all()) -> string().
ensure_dir_deleted(RepoDir) -> run_command(["rm -r ", RepoDir]).

ensure_dir(RepoDir) ->
  case filelib:ensure_dir(RepoDir) of
    ok -> ok;
    {error, Error} -> throw(Error)
  end.

-spec run_command(iodata()) -> string().
run_command(Command) ->
  lager:info("~s", [Command]),
  Result = os:cmd(Command),
  HR = lists:duplicate(80, $~),
  lager:debug("~n~s~n$ ~s~n~s~n~s", [HR, Command, Result, HR]),
  Result.

-spec unique_id() -> binary().
unique_id() ->
  {X, Y, Z} = os:timestamp(),
  iolist_to_binary(io_lib:format("~7.10.0B-~7.10.0B-~7.10.0B", [X, Y, Z])).

-spec compile_project(file:name_all()) -> [string()].
compile_project(RepoDir) ->
  Output =
    case filelib:is_regular(filename:join(RepoDir, "Makefile")) of
      true -> make_project(RepoDir);
      false ->
        case filelib:is_regular(filename:join(RepoDir, "rebar.config")) of
          true -> rebarize_project(RepoDir);
          false ->
            lager:warning(
              "No Makefile nor rebar.config in ~p:\n\t~s",
              [RepoDir, filelib:wildcard(filename:join(RepoDir, "*"))]),
            throw(cant_compile)
        end
    end,
  DecodedOutput = unicode:characters_to_binary(Output),
  try
    re:split(DecodedOutput, "\n", [{return, binary}, trim])
  catch
    _:Error ->
      lager:warning("Uncomprehensible output: ~p", [DecodedOutput]),
      Error
  end.

make_project(RepoDir) -> run_command(["cd ", RepoDir, "; V=1000 make"]).

rebarize_project(RepoDir) ->
  Rebar =
    case os:find_executable("rebar") of
      false -> filename:absname("deps/rebar/rebar");
      Exec -> Exec
    end,
  run_command(["cd ", RepoDir, "; ", Rebar, " --verbose get-deps compile"]),
  run_command(["cd ", RepoDir, "; ", Rebar, " skip_deps=true clean compile"]).

-spec messages_from_comments(string(), [comment()], [egithub_webhook:file()]) ->
  [egithub_webhook:message()].
messages_from_comments(ToolName, Comments, GithubFiles) ->
  lists:flatmap(
    fun(Comment) ->
      messages_from_comment(ToolName, Comment, GithubFiles)
    end, Comments).

messages_from_comment(ToolName, Comment, GithubFiles) ->
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
      FullText = format_message(ToolName, Text),
      messages_from_comment(File, Line, FullText, MatchingFile)
  end.

messages_from_comment(Filename, 0, Text, File) ->
  #{<<"raw_url">> := RawUrl} = File,
  [ #{commit_id => commit_id_from_raw_url(RawUrl, Filename),
      path      => Filename,
      position  => 0,
      text      => Text
      }
  ];
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
commit_id_from_raw_url(Url, Filename) ->
  Regex = <<".+/raw/(.+)/", Filename/binary>>,
  {match, [CommitId]} = re:run(Url, Regex, [{capture, all_but_first, binary}]),
  binary_to_list(CommitId).

-spec format_message(string(), iodata()) -> binary().
format_message(ToolName, Text) ->
  iolist_to_binary(["According to **", ToolName, "**:\n> ", Text]).
