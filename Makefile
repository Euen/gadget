PROJECT = gadget

CONFIG ?= config/app.config

DEPS = sync elvis cowboy lager erlydtl merl egithub shotgun eper rebar xref_runner katana sumo epocxy

dep_sync = git git://github.com/inaka/sync.git 0.1.3
dep_eper = git git://github.com/massemanet/eper.git 0.90.0
dep_egithub = git git://github.com/inaka/erlang-github.git 0.1.12
dep_elvis = git git://github.com/inaka/elvis.git 0.2.5-alpha
dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.1
dep_lager = git git://github.com/basho/lager.git 2.1.1
dep_erlydtl = git git://github.com/erlydtl/erlydtl.git 0.10.0
dep_merl = git git://github.com/richcarl/merl.git cc816c38
dep_shotgun = git git://github.com/inaka/shotgun.git 0.1.8
dep_rebar = git git://github.com/basho/rebar.git 2.0.0
dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.1
dep_katana =  git git://github.com/inaka/erlang-katana.git 0.2.4
dep_sumo = git git://github.com/inaka/sumo_db.git 0.3.5
dep_epocxy = git git://github.com/duomark/epocxy.git 0.9.8e

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 131072}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

include erlang.mk

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = gadget
CT_OPTS = -cover test/gadget.coverspec  -erl_args -config config/test

SHELL_OPTS= -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config ${CONFIG}
