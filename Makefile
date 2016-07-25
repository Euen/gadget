PROJECT = gadget

CONFIG ?= config/app.config
ERLDOCS ?= ./erldocs

DEPS = elvis katana cowboy lager erlydtl egithub shotgun eper xref_runner sumo_db epocxy jiffy

TEST_DEPS = mixer katana_test meck
SHELL_DEPS = sync
LOCAL_DEPS = tools compiler syntax_tools common_test inets test_server dialyzer wx mnesia

dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.8
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_eper = git https://github.com/massemanet/eper.git 0.96.4
dep_egithub = hex 0.2.8
dep_elvis = git https://github.com/inaka/elvis.git 0.2.11
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.4
dep_lager = git https://github.com/basho/lager.git 3.2.0
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git 0.11.1
dep_shotgun = git https://github.com/inaka/shotgun.git 0.2.0
dep_rebar = git https://github.com/erlang/rebar3.git 3.1.0
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.6
dep_sumo_db = git https://github.com/inaka/sumo_db.git 0.5.0
dep_epocxy = git https://github.com/duomark/epocxy.git 1.0.0
dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.23
dep_katana_test = git https://github.com/inaka/katana-test.git 0.0.5
dep_meck = git https://github.com/eproxus/meck.git 0.8.4

include erlang.mk

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 131072}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

EDOC_OPTS := {source_path, ["src", "src/handlers", "src/models", "src/slave_nodes", "src/webhooks"]},
EDOC_OPTS += {application, gadget}, {subpackages, false}

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info
CT_OPTS = -cover test/gadget.coverspec -erl_args -config ${CONFIG}

SHELL_OPTS= -name ${PROJECT}@127.0.0.1 -s sync -s ${PROJECT} -config ${CONFIG}
RUN_OPTS= -name gadget_internal@127.0.0.1 -setcookie gadget_internal -s ${PROJECT} -config ${CONFIG} -noshell

erldocs: app
	${ERLDOCS} src/* -o docs/

testshell:
	erl -pa ebin -pa deps/*/ebin -pa test config ${CONFIG} -s sync

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze

# Remove this override once PR 361 is merged and replace with
# (https://github.com/ninenines/erlang.mk/pull/361)
ERLYDTL_OPTS += debug_info, {parse_transform, lager_transform}

define erlydtl_compile.erl
        [begin
                Module0 = case "$(strip $(DTL_FULL_PATH))" of
                        "" ->
                                filename:basename(F, ".dtl");
                        _ ->
                                "$(DTL_PATH)" ++ F2 = filename:rootname(F, ".dtl"),
                                re:replace(F2, "/",  "_",  [{return, list}, global])
                end,
                Module = list_to_atom(string:to_lower(Module0) ++ "$(DTL_SUFFIX)"),
                case erlydtl:compile(F, Module, [$(ERLYDTL_OPTS)] ++ [{out_dir, "ebin/"}, return_errors, {doc_root, "templates"}]) of
                        ok -> ok;
                        {ok, _} -> ok
                end
        end || F <- string:tokens("$(1)", " ")],
        halt().
endef
