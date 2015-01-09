PROJECT = gadget

CONFIG ?= config/app.config

DEPS = elvis cowboy lager sync erlydtl merl egithub shotgun eper

dep_eper = git git://github.com/massemanet/eper.git 0.90.0
dep_elvis = git git://github.com/inaka/elvis.git 0.2.5
dep_cowboy = git git://github.com/extend/cowboy.git 1.0.0
dep_lager = git git://github.com/basho/lager.git 2.1.0
dep_sync = git git://github.com/inaka/sync.git 0.1
dep_erlydtl = git git://github.com/erlydtl/erlydtl 0.9.4
dep_merl = git git://github.com/richcarl/merl master
dep_egithub = git git://github.com/inaka/erlang-github 0.1.4
dep_shotgun = git git://github.com/inaka/shotgun 0.1.6

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = gadget
CT_OPTS = -cover test/gadget.coverspec  -erl_args -config config/test

SHELL_OPTS= -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config ${CONFIG}
