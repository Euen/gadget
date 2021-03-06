%%% @doc General utilities for the project
-module(gadget_utils).

-export([ active_tools/1
        , tool_info/3
        , is_public/1
        , is_admin/1
        , is_supported/1
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
        , save_status_log/4
        , report_error/6
        , catch_error_source/6
        , extract_errors/1
        , extract_comments/1
        , build_tool_type/1
        , default_verbosity/1
        , exists_file_in_repo/2
        , rebar3_command_path/1
        , get_username/1
        , now_datetime/0
        ]).

-type comment() :: #{file   => string(),
                     number => pos_integer(),
                     text   => binary()
                    }.
-type webhook_info() :: #{ tool => tool()
                         , mod => atom()
                         , name => string()
                         , context => string()
                         }.
-type tool_info() :: #{ name => atom()
                      , status => on | off
                      , hook_id => integer() | undefined
                      }.
-type tool() :: xref | elvis | compiler | dialyzer | lewis.
-type buildtool() :: makefile | rebar3.

-export_type([webhook_info/0, comment/0, tool_info/0, tool/0]).


%% @doc Retrieves the list of active webhook tools
-spec active_tools([gadget_repo_hooks:repo_hook()]) -> [tool_info()].
active_tools(Hooks) ->
  Tools = application:get_env(gadget, webhooks, #{}),
  [tool_info(ToolName, Tools, Hooks) || ToolName <- maps:keys(Tools)].

%% @doc Retrieves information about a tool related to a particular repo
-spec tool_info(atom(), map(), [gadget_repo_hooks:repo_hook()]) -> tool_info().
tool_info(ToolName, _Tools, []) ->
  #{ name => ToolName
   , status => off
   , hook_id => undefined
   };
tool_info(ToolName, Tools, Hooks) ->
  ToolUrl = maps:get(url, maps:get(ToolName, Tools)),
  Fun =
    fun (RepoHook) ->
          list_to_binary(ToolUrl) == gadget_repo_hooks:url(RepoHook)
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
        gadget_repo_hooks:id(hd(FilteredHooks));
      off -> undefined
    end,

  #{ name => ToolName
   , status => Status
   , hook_id => HookId
   }.

%% @doc is the repo public?
-spec is_public(Repo::gadget_repos:repo()) -> boolean().
is_public(Repo) ->
  not gadget_repos:private(Repo).

%% @doc is the user an admin for that organization repo?
-spec is_admin(Repo::gadget_repos:repo()) -> boolean().
is_admin(Repo) ->
  gadget_repos:admin(Repo) =:= true.

%% @doc is this repository language supported by gadget?
-spec is_supported(Repo::gadget_repos:repo()) ->
  {true, gadget_repos:repo()} | boolean().
is_supported(Repo) ->
  case gadget_repos:language(Repo) of
    null ->
      {true, Repo};
    <<"null">> ->
      {true, Repo};
    Language ->
      {ok, Tools} = application:get_env(gadget, webhooks),
      ToolsLangs = lists:foldl(fun(#{languages := Langs}, Acc) ->
                                 Acc ++ Langs
                               end,
                               [],
                               maps:values(Tools)),
      % Remove repeated values
      SupportedLangs = lists:usort(ToolsLangs),
      case lists:member(Language, SupportedLangs) of
        true ->
          {true, gadget_repos:languages(Repo, [Language])};
        false ->
          languages_intersect(Repo, SupportedLangs)
      end
  end.

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
    case exists_file_in_repo(RepoDir, "rebar.config") of
      true -> rebarize_project(RepoDir, Verbosity);
      false ->
        case exists_file_in_repo(RepoDir, "Makefile") of
          true -> make_project(RepoDir, Verbosity);
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
  % If rebar is included in the repo, use it.
  RebarIncluded = exists_file_in_repo(RepoDir, "rebar"),
  case RebarIncluded of
    true ->
      Rebar = filename:join(RepoDir, "rebar"),
      VerbOption = rebar_verbosity(Verbosity),
      % Compiles everything (deps and app).
      _ = run_command(["cd ", RepoDir, "; ",
                       Rebar, VerbOption, " get-deps compile"]),
      % Just like the last command but avoid getting deps warnings, just
      % app warnings are important here.
      run_command(["cd ", RepoDir, "; ",
                   Rebar, " skip_deps=true clean compile"]);
    false ->
      VerbOption = rebar3_verbosity(Verbosity),
      Rebar = rebar3_command_path(RepoDir),
      run_command(["cd ", RepoDir, "; ", VerbOption, Rebar, " compile"])
  end.

-spec rebar3_command_path(file:name_all()) -> string().
rebar3_command_path(RepoDir) ->
  Rebar3Included = exists_file_in_repo(RepoDir, "rebar3"),
  case Rebar3Included of
    true -> filename:join(RepoDir, "rebar3");
    false ->
      Rebar3Path = run_command(["which rebar3"]),
      % Remove new line character
      string:substr(Rebar3Path, 1, length(Rebar3Path) - 1)
  end.

-spec default_verbosity(makefile | rebar | rebar3) -> string().
default_verbosity(BuildTool) ->
  Verbosity = application:get_env(gadget, default_verbosity, silent),
  case BuildTool of
    makefile ->
      make_verbosity(Verbosity);
    rebar ->
      rebar_verbosity(Verbosity);
    rebar3 ->
      rebar3_verbosity(Verbosity)
  end.

make_verbosity(verbose) -> "V=2 ";
make_verbosity(silent) -> "".

rebar_verbosity(verbose) -> " --verbose";
rebar_verbosity(silent) -> "".

rebar3_verbosity(verbose) -> " DEBUG=1 TERM=dumb QUIET=1 ";
rebar3_verbosity(silent) -> " TERM=dumb ".

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
     || #{ <<"filename">>  := FileName
         , <<"status">>    := Status
         } = GithubFile <- GithubFiles
          , true == ends_with(File, FileName)
          , Status /= <<"deleted">>
    ],
  case MatchingFiles of
    [] -> [];
    [MatchingFile|_] ->
      FullText = format_message(ToolName, Text),
      #{<<"filename">> := FileName} = MatchingFile,
      messages_from_comment(FileName, Line, FullText, MatchingFile)
  end.

ends_with(Big, Small) when is_list(Big) ->
  BigBinary = list_to_binary(Big),
  ends_with(BigBinary, Small);
ends_with(Big, Small) when is_list(Small) ->
  SmallBinary = list_to_binary(Small),
  ends_with(Big, SmallBinary);
ends_with(Big, Small) ->
  LBig = erlang:size(Big),
  LSmall = erlang:size(Small),
  LRest = LBig - LSmall,
  case Big of
    <<_:LRest/binary, Small/binary>> -> true;
    _Other -> false
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

-spec save_status_log(atom(), string(), string(), integer()) -> string().
save_status_log(Tool, Lines, Repo, PrNumber) ->
  Log = gadget_logs_repo:create(Tool, Repo, PrNumber, Lines),
  Id = gadget_logs:id(Log),
  status_details_url(Tool, PrNumber, Id).

-spec report_error(atom(), list(), string(), integer(), string(), integer()) ->
  {error, {failed, integer()}, string()} | {ok, [map()], string()}.
report_error(Tool, [], Repo, ExitStatus, Lines, Number) ->
  DetailsUrl = save_status_log(Tool, Lines, Repo, Number),
  {error, {failed, ExitStatus}, DetailsUrl};
report_error( Tool, [#{commit_id := CommitId} | _] = Messages, Repo, ExitStatus
            , Lines, Number) ->
  Text = io_lib:format( "**~p** failed with exit status: ~p"
                      , [Tool, ExitStatus]),
  ExtraMessage =
    #{commit_id => CommitId,
      path      => "",
      position  => 0,
      text      => list_to_binary(Text)
     },
  DetailsUrl = save_status_log(Tool, Lines, Repo, Number),
  {ok, [ExtraMessage | Messages], DetailsUrl}.

-spec catch_error_source(Output::string(),
                         ExitStatus::integer(),
                         Tool::tool(),
                         GithubFiles::[egithub_webhook:file()],
                         RepoName::string(),
                         Number::integer()) ->
  {error, {failed, integer()}, string()} |
  {ok, [map()], string()}.
catch_error_source(Output, ExitStatus, Tool, GithubFiles, RepoName, Number) ->
  Lines = output_to_lines(Output),
  case error_source(Lines, Tool) of
    unknown ->
      DetailsUrl = save_status_log(Tool, Output, RepoName, Number),
      {error, {failed, ExitStatus}, DetailsUrl};
    Tool ->
      Comments = extract_errors(Lines),
      ToolName = capitalize(atom_to_binary(Tool, utf8)),
      Messages = messages_from_comments(ToolName, Comments, GithubFiles),
      report_error(Tool, Messages, RepoName, ExitStatus, Output, Number)
  end.

-spec extract_errors(Lines ::[list()]) -> [map()].
extract_errors(Lines) ->
  {ok, Regex} = re:compile(<<"(.+):([0-9]*): (.+)">>),
  extract_errors(Lines, Regex, []).
extract_errors([], _Regex, Errors) -> Errors;
extract_errors([Line|Lines], Regex, Errors) ->
  NewErrors =
    case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
      {match, [File, <<>>, Comment]} ->
        [#{ file   => File
          , number => 0
          , text   => Comment
          } | Errors];
      {match, [File, Number, Comment]} ->
        [#{ file   => File
          , number => binary_to_integer(Number)
          , text   => Comment
          } | Errors];
      {match, Something} ->
        _ = lager:error("WHAT? ~p", [Something]),
        [];
      _ ->
        Errors
    end,
  extract_errors(Lines, Regex, NewErrors).

-spec extract_comments(list()) -> [map()].
extract_comments(Lines) ->
  {ok, Regex} = re:compile(<<"\s([0-9]*): (.+)$">>),
  extract_comments(Lines, Regex, [], undefined).

extract_comments([], _Regex, Errors, _File) -> Errors;
extract_comments([Line | Lines], Regex, Errors, File) ->
  case re:run(Line, Regex, [{capture, [1, 2], list}]) of
    {match, [LineNumber, Comments]} ->
      Number = list_to_integer(LineNumber),
      NewError = #{file => File, number => Number, text => Comments},
      extract_comments(Lines, Regex, [NewError | Errors], File);
    nomatch ->
      %% Line contains the file with dialyze comments.
      %% the next items are comments related with this line until the next File.
      extract_comments(Lines, Regex, Errors, Line)
  end.


-spec error_source(Lines::[binary()], Tool::tool()) -> tool() | unknown.
error_source(_Lines, xref = Tool) -> Tool;
error_source(_Lines, elvis = Tool) -> Tool;
error_source(_Lines, lewis = Tool) -> Tool;
error_source(Lines, Tool) ->
  LastLines = lists:sublist(lists:reverse(Lines), 3),
  Regexes = ["make.*?[:] [*][*][*] [[][^]]*[]] Error",
             "ERROR[:] compile failed",
             "Compiling .* failed$",
             "Dialyzer works only for *",
             "===> Error in dialyzing apps:*",
             "{badmatch,{error*"
             "Not * found"],
  MatchesRegexes =
    fun(Line) ->
      lists:any(fun(Regex) -> nomatch /= re:run(Line, Regex) end, Regexes)
    end,
  case lists:any(MatchesRegexes, LastLines) of
    true -> Tool;
    false -> rebar_regex(Lines, Tool)
  end.

rebar_regex(Lines, Tool) ->
  Regex = ".*===> Compilation failed.*",
  case lists:any(fun(Line) -> nomatch /= re:run(Line, Regex) end, Lines) of
    true -> Tool;
    false -> unknown
  end.

-spec build_tool_type(file:name_all()) -> buildtool().
build_tool_type(RepoDir) ->
  ErlangMkIncluded = exists_file_in_repo(RepoDir, "erlang.mk"),
  case ErlangMkIncluded of
    true -> makefile;
    false ->
      RebarConfigIncluded = exists_file_in_repo(RepoDir, "rebar.config"),
      case RebarConfigIncluded of
        true -> rebar3;
        false -> throw(
                   {error, {status, 1, "Not rebar.config or erlang.mk found"}}
                 )
      end
  end.

-spec exists_file_in_repo(file:name_all(), string()) -> boolean().
exists_file_in_repo(RepoDir, FileName) ->
  filelib:is_file(filename:join(RepoDir, FileName)).

-spec get_username(User::map()) ->
  binary().
get_username(User) ->
  Name = maps:get(<<"name">>, User, null),
  case Name of
    null -> maps:get(<<"login">>, User);
    Name1 -> Name1
  end.

-spec now_datetime() ->
  gadget_repos:datetime().
now_datetime() ->
  calendar:universal_time().

%% =============================================================================
%% Private
%% =============================================================================

-spec languages_intersect(Repo::gadget_repos:repo(),
                          SupportedLangs::gadget_repos:languages()) ->
  {true, gadget_repos:repo()} | false.
languages_intersect(Repo, SupportedLangs) ->
  RepoLangs = repo_langs(Repo),
  case RepoLangs -- (RepoLangs -- SupportedLangs) of
    [] -> false;
    Langs -> {true, gadget_repos:languages(Repo, Langs)}
  end.

-spec repo_langs(Repo::gadget_repos:repo()) ->
  gadget_repos:languages().
repo_langs(Repo) ->
  RepoId = gadget_repos:id(Repo),
  gadget_repos_repo:languages(RepoId).
