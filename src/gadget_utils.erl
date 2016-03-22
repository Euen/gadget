%%% @doc General utilities for the project
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
        , compile_project/2
        , messages_from_comments/3
        , format_message/2
        , webhook_info/1
        , output_to_lines/1
        , status_details_url/3
        , save_status_log/2
        ]).

-type comment() :: #{file   => string(),
                     number => pos_integer(),
                     text   => binary()
                    }.
-type webhook_info() :: #{ tool => atom()
                         , mod => atom()
                         , name => string()
                         , context => string()
                         }.
-type tool_info() :: #{ name => atom()
                      , status => on | off
                      , hook_id => binary()
                      }.
-export_type([webhook_info/0, comment/0, tool_info/0]).

%% @doc Retrieves the list of active webhook tools
-spec active_tools([map()]) -> [tool_info()].
active_tools(Hooks) ->
  Tools = application:get_env(gadget, webhooks, #{}),
  [tool_info(ToolName, Tools, Hooks) || ToolName <- maps:keys(Tools)].

%% @doc Retrieves information about a tool related to a particular repo
-spec tool_info(atom(), map(), [map()]) -> tool_info().
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
   , hook_id => HookId
   }.

%% @doc is the repo public?
-spec is_public(map()) -> boolean().
is_public(#{<<"private">> := Private}) -> not Private.

%% @doc is the user an admin for that organization repo?
-spec is_admin(map()) -> boolean().
is_admin(#{<<"permissions">> := #{<<"admin">> := true}}) -> true;
is_admin(_Repo) -> false.

%% @doc make sure that there is a directory where to clone the repository
-spec ensure_repo_dir(binary()) -> file:name_all().
ensure_repo_dir(RepoName) ->
  TmpRoot = application:get_env(gadget, tmp_path, "/tmp/gadget"),
  Now = unique_id(),
  RepoDir = binary_to_list(filename:join([TmpRoot, RepoName, Now])),
  _ = ensure_dir_deleted(RepoDir),
  ensure_dir(RepoDir),
  RepoDir.

%% @doc clones the github repository in the specified path
-spec clone_repo(file:name_all(), binary(), binary()) -> ok.
clone_repo(RepoDir, Branch, GitUrl) ->
  Command =
    io_lib:format(
      "git clone -v -b ~s ~s ~s", [Branch, GitUrl, RepoDir]),
  _ = run_command(Command),
  ok.

%% @doc makes sure that a directory is deleted
-spec ensure_dir_deleted(file:name_all()) -> string().
ensure_dir_deleted(RepoDir) -> run_command(["rm -rf ", RepoDir]).

ensure_dir(RepoDir) ->
  case filelib:ensure_dir(RepoDir) of
    ok -> ok;
    {error, Error} -> throw(Error)
  end.

%% @doc runs a system command using os:cmd/1
-spec run_command(iodata()) -> string().
run_command(Command) ->
  HR = lists:duplicate(80, $~),
  _ = lager:info(HR),
  _ = lager:info("$ ~s", [Command]),
  Opts =  #{log_fun => fun(X) -> _ = lager:info("~s", [X]) end},
  case ktn_os:command(Command, Opts) of
    {0, Result} ->
      _ = lager:info(HR),
      Result;
    {ExitStatus, Result} ->
      _ = lager:info(HR),
      throw({error, {status, ExitStatus, Result}})
  end.

%% @doc generates a "unique" id based on os:timestamp/0
-spec unique_id() -> binary().
unique_id() ->
  {X, Y, Z} = os:timestamp(),
  iolist_to_binary(io_lib:format("~7.10.0B-~7.10.0B-~7.10.0B", [X, Y, Z])).

%% @doc runs make or rebar get-deps compile on a project
-spec compile_project(file:name_all(), verbose | silent) -> [string()].
compile_project(RepoDir, Verbosity) ->
  Output =
    case filelib:is_regular(filename:join(RepoDir, "Makefile")) of
      true -> make_project(RepoDir, Verbosity);
      false ->
        case filelib:is_regular(filename:join(RepoDir, "rebar.config")) of
          true -> rebarize_project(RepoDir, Verbosity);
          false ->
            _ = lager:warning(
              "No Makefile nor rebar.config in ~p:\n\t~s",
              [RepoDir, filelib:wildcard(filename:join(RepoDir, "*"))]),
            throw(cant_compile)
        end
    end,
  output_to_lines(Output).

make_project(RepoDir, verbose) ->
  run_command(["cd ", RepoDir, "; V=2 make"]);
make_project(RepoDir, silent) -> run_command(["cd ", RepoDir, "; make"]).

rebarize_project(RepoDir, Verbosity) ->
  Rebar =
    case os:find_executable("rebar") of
      false -> filename:absname("deps/rebar/rebar");
      Exec -> Exec
    end,
  VerbOption =
    case Verbosity of
      verbose -> " --verbose";
      silent -> ""
    end,
  _ =
    run_command(["cd ", RepoDir, "; ", Rebar, VerbOption, " get-deps compile"]),
  _ =
    run_command(["cd ", RepoDir, "; ", Rebar, " skip_deps=true clean compile"]).

%% @doc generates egithub_webhook:messages from a list of comments
-spec messages_from_comments(string(), [comment()], [egithub_webhook:file()]) ->
  [egithub_webhook:message()].
messages_from_comments(ToolName, Comments, GithubFiles) ->
  lists:flatmap(
    fun(Comment) ->
      messages_from_comment(ToolName, Comment, GithubFiles)
    end, Comments).

messages_from_comment(ToolName, #{file := <<>>} = Comment, GithubFiles) ->
  #{text := Text} = Comment,
  [#{<<"filename">> := FileName} = FirstFile|_] = GithubFiles,
  FullText = format_message(ToolName, Text),
  messages_from_comment(FileName, 0, FullText, FirstFile);
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
messages_from_comment(Filename,
                      Line,
                      Text,
                      #{<<"patch">> := Patch, <<"raw_url">> := RawUrl}) ->
  case elvis_git:relative_position(Patch, Line) of
    {ok, Position} ->
      [ #{commit_id => commit_id_from_raw_url(RawUrl, Filename),
          path      => Filename,
          position  => Position,
          text      => Text
         }
      ];
    not_found ->
      _ = lager:info("Line ~p does not belong to file's diff.", [Line]),
      []
  end;
messages_from_comment(Filename, _Line, Text, File) ->
  messages_from_comment(Filename, 0, Text, File).

%% @doc Gets a raw_url for a file and extracts the commit id from it.
commit_id_from_raw_url(Url, Filename) ->
  Regex = <<".+/raw/(.+)/", Filename/binary>>,
  {match, [CommitId]} = re:run(Url, Regex, [{capture, all_but_first, binary}]),
  binary_to_list(CommitId).

%% @doc runs a system command using os:cmd/1
-spec format_message(string(), iodata()) -> binary().
format_message(ToolName, Text) ->
  iolist_to_binary(["According to **", ToolName, "**:\n> ", Text]).

-spec webhook_info(binary()) -> webhook_info().
webhook_info(Tool) ->
  #{ tool => binary_to_atom(Tool, utf8)
   , mod => binary_to_atom(<<"gadget_", Tool/binary, "_webhook">>, utf8)
   , name => capitalize(Tool)
   , context => binary_to_list(<<"gadget/", Tool/binary>>)
   }.

capitalize(<<>>) -> <<>>;
capitalize(<<C, Rest/binary>>) ->
  [Upper] = string:to_upper([C]),
  [Upper | binary_to_list(Rest)].

-spec output_to_lines(string()) -> [binary()] | any().
output_to_lines(Output) ->
  DecodedOutput = unicode:characters_to_binary(Output),
  try
    re:split(DecodedOutput, "\n", [{return, binary}, trim])
  catch
    _:Error ->
      _ = lager:warning("Uncomprehensible output: ~p", [DecodedOutput]),
      Error
  end.

-spec status_details_url(atom(), integer(), integer()) -> string().
status_details_url(Tool, PrNumber, Id) ->
  {ok, StatusDetailsUrl} = application:get_env(gadget, status_details_url),
  lists:flatten(
      io_lib:format("~s~p/~p/~p", [StatusDetailsUrl, PrNumber, Tool, Id])).

-spec save_status_log(string(), atom()) -> string().
save_status_log(Lines, Number) ->
  #{id := Id} = gadget_logs_repo:create(compiler, Number, Lines),
  gadget_utils:status_details_url(compiler, Number, Id).