-module(gadget_repos).

-behaviour(sumo_doc).

-export([new/11,
         %% Getters
         id/1,
         name/1,
         full_name/1,
         html_url/1,
         private/1,
         admin/1,
         pull/1,
         push/1,
         languages_url/1,
         language/1,
         languages/1,
         created_at/1,
         updated_at/1,
         %% Setters
         languages/2
        ]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%% Types

-type id()            :: integer().
-type name()          :: binary().
-type full_name()     :: binary().
-type html_url()      :: binary().
-type private()       :: boolean().
-type admin()         :: boolean(). % permissions
-type pull()          :: boolean(). % permissions
-type push()          :: boolean(). % permissions
-type languages_url() :: binary().
-type language()      :: binary().
-type languages()     :: [language()].
-type created_at()    :: datetime().
-type updated_at()    :: datetime().


-opaque repo() ::
  #{id => id(),
    name => name(),
    full_name => full_name(),
    html_url => html_url(),
    private => private(),
    admin => admin(),
    pull => pull(),
    push => push(),
    languages_url => languages_url(),
    language => language(),
    languages => languages(),
    created_at => created_at(),
    updated_at => updated_at()}.

-export_type([repo/0,
              id/0,
              name/0,
              full_name/0,
              private/0,
              languages_url/0,
              language/0,
              languages/0]).

-type datetime() ::
          {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()}
          }.
-export_type([datetime/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(),
          Name::name(),
          FName::full_name(),
          HtmlUrl::html_url(),
          Priv::private(),
          Admin::admin(),
          Pull::pull(),
          Push::push(),
          LangsUrl::languages_url(),
          Lang::language(),
          Langs::languages()) ->
  repo().
new(Id, Name, FName, HtmlUrl, Priv, Admin, Pull, Push, LangsUrl, Lang, Langs) ->
  Now = gadget_utils:now_datetime(),
  #{id => Id,
    name => Name,
    full_name => FName,
    html_url => HtmlUrl,
    private => Priv,
    admin => Admin,
    pull => Pull,
    push => Push,
    languages_url => LangsUrl,
    language => Lang,
    languages => Langs,
    created_at => Now,
    updated_at => Now
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Getters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec id(Repo::repo()) -> integer().
id(Repo) ->
  maps:get(id, Repo).

-spec name(Repo::repo()) -> name().
name(Repo) ->
  maps:get(name, Repo).

-spec full_name(Repo::repo()) -> full_name().
full_name(Repo) ->
  maps:get(full_name, Repo).

-spec html_url(Repo::repo()) -> html_url().
html_url(Repo) ->
  maps:get(html_url, Repo).

-spec private(Repo::repo()) -> private() | integer().
private(Repo) ->
  maps:get(private, Repo).

-spec admin(Repo::repo()) -> admin() | integer().
admin(Repo) ->
  maps:get(admin, Repo).

-spec pull(Repo::repo()) -> pull() | integer().
pull(Repo) ->
  maps:get(pull, Repo).

-spec push(Repo::repo()) -> boolean() | integer().
push(Repo) ->
  maps:get(push, Repo).

-spec languages_url(Repo::repo()) -> languages_url().
languages_url(Repo) ->
  maps:get(languages_url, Repo).

-spec language(Repo::repo()) -> language() | null.
language(Repo) ->
  maps:get(language, Repo).

-spec languages(Repo::repo()) -> languages().
languages(Repo) ->
  maps:get(languages, Repo).

-spec created_at(Repo::repo()) -> created_at().
created_at(Repo) ->
  maps:get(created_at, Repo).

-spec updated_at(Repo::repo()) -> updated_at().
updated_at(Repo) ->
  maps:get(updated_at, Repo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Setters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec languages(Repo::repo(), Languages::languages()) -> repo().
languages(Repo, Languages) ->
  Repo#{languages => Languages}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,            integer,  [id, not_null]),
     sumo:new_field(name,          string,   [{length, 255}]),
     sumo:new_field(full_name,     string,   [{length, 255}]),
     sumo:new_field(html_url,      string,   [{length, 255}]),
     sumo:new_field(private,       binary),
     sumo:new_field(admin,         binary),
     sumo:new_field(pull,          binary),
     sumo:new_field(push,          binary),
     sumo:new_field(languages_url, string,   [{length, 255}]),
     sumo:new_field(language,      string,   [{length, 255}]),
     sumo:new_field(languages,     binary),
     sumo:new_field(created_at,    datetime, [not_null]),
     sumo:new_field(updated_at,    datetime, [not_null])
    ],
  sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(Repo::repo()) -> sumo:doc().
sumo_sleep(Repo) ->
  Repo#{languages => term_to_binary(maps:get(languages, Repo)),
        private => term_to_binary(maps:get(private, Repo)),
        admin => term_to_binary(maps:get(admin, Repo)),
        pull => term_to_binary(maps:get(pull, Repo)),
        push => term_to_binary(maps:get(push, Repo))}.

-spec sumo_wakeup(Doc::sumo:doc()) -> repo().
sumo_wakeup(Doc) ->
  Doc#{languages => binary_to_term(maps:get(languages, Doc)),
       private => binary_to_term(maps:get(private, Doc)),
       admin => binary_to_term(maps:get(admin, Doc)),
       pull => binary_to_term(maps:get(pull, Doc)),
       push => binary_to_term(maps:get(push, Doc))}.
