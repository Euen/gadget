.PHONY: all build publish

DOCKER         ?= $(shell which docker)
COMPOSE        ?= $(shell which docker-compose)

DKR_REGISTRY   ?= registry.inakalabs.com
DKR_IMAGE      ?= gadget
DKR_TAG        ?= latest

DKR_BUILD_OPTS ?= -t $(DKR_REGISTRY)/$(DKR_IMAGE):$(DKR_TAG)


build: clean ## Build docker image
	$(COMPOSE) build

publish: ## Publish docker image to the provided container registry
	$(DOCKER) build $(DKR_BUILD_OPTS) .
	$(DOCKER) push $(DKR_REGISTRY)/$(DKR_IMAGE):$(DKR_TAG)

start: stop
	$(COMPOSE) up -d

stop:
	$(COMPOSE) down

logs:
	$(DOCKER) logs `$(DOCKER) ps | grep gadget_web | cut -d " " -f 1 `

attach:
	$(DOCKER) exec -ti `$(DOCKER) ps | grep gadget_web | cut -d " " -f 1 ` bash

clean:
	rm -rf _build compile_commands.json rebar.lock
