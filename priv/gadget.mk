include Makefile

DIALYZER_DIRS ?= --src -r src

gadget-plt:
	@(make plt) || \
	if [ ! -f '$(DIALYZER_PLT)' ]; then \
		echo "Error: PLT file could not be generated"; \
		(exit 2); \
	else \
		echo "PLT was generated"; \
		(exit 0); \
	fi


ifneq ($(wildcard $(DIALYZER_PLT)),)
gadget-dialyze:
else
gadget-dialyze: gadget-plt
endif
	$(gen_verbose) dialyzer --no_native --no_check_plt --raw --quiet \
		$(DIALYZER_OPTS/--verbose/) $(DIALYZER_DIRS) > gadget_dialyze.result; \
		echo "ok"
