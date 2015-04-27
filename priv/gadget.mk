DIALYZER_DIRS ?= --src -r src

include Makefile

ifneq ($(wildcard $(DIALYZER_PLT)),)
gadget-dialyze:
else
gadget-dialyze: $(DIALYZER_PLT)
endif
	$(gen_verbose) dialyzer --no_native --no_check_plt --raw --quiet \
		$(DIALYZER_OPTS/--verbose/) $(DIALYZER_DIRS) > gadget-dialyze.result
