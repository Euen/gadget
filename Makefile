PROJECT = gadget

DEPS = elvis cowboy lager sync erlydtl merl egithub
TEST_DEPS = meck

dep_elvis = git https://github.com/inaka/elvis.git 0.2.3
dep_cowboy = git https://github.com/extend/cowboy.git 0.10.0
dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_sync = git https://github.com/rustyio/sync.git master
dep_meck = git https://github.com/eproxus/meck master
dep_erlydtl = git https://github.com/erlydtl/erlydtl 0.9.4
dep_merl = git https://github.com/richcarl/merl master
dep_egithub = git https://github.com/inaka/erlang-github 0.1.1

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = gadget
CT_OPTS = -cover test/gadget.coverspec  -erl_args -config config/test

SHELL_OPTS= -s sync -s lager -s gadget_server -config config/app.config
