SHELL := /bin/bash

ligo_compiler?=docker run -u $(id -u):$(id -g) --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:stable
# ^ Override this variable when you run make command by make <COMMAND> ligo_compiler=<LIGO_EXECUTABLE>
# ^ Otherwise use default one (you'll need docker)
PROTOCOL_OPT?=

project_root=--project-root .
# ^ required when using packages

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

compile = $(ligo_compiler) compile contract $(project_root) ./src/$(1) -o ./compiled/$(2) $(3) $(PROTOCOL_OPT)
# ^ compile contract to michelson or micheline

test = $(ligo_compiler) run test $(project_root) ./test/$(1) $(PROTOCOL_OPT)
# ^ run given test file

compile: ## compile contracts
	@if [ ! -d ./compiled ]; then mkdir ./compiled ; fi
	@$(call compile,main.mligo,taco_shop_token.tz)
	@$(call compile,main.mligo,taco_shop_token.json,--michelson-format json)

clean: ## clean up
	@rm -rf compiled

deploy: ## deploy
	@if [ ! -f ./scripts/metadata.json ]; then cp scripts/metadata.json.dist \
        scripts/metadata.json ; fi
	@cd scripts && npx ts-node ./deploy.ts

install: ## install dependencies
	@if [ ! -f ./.env ]; then cp .env.dist .env ; fi
	@$(LIGO) install
	@cd scripts && npm i

.PHONY: test
test: ## run tests (SUITE=permit make test)
ifndef SUITE
	@$(call test,permit.test.mligo)
	@$(call test,set_expiry.test.mligo)
	@$(call test,set_admin.test.mligo)
	@$(call test,transfer.test.mligo)
	@$(call test,create_token.test.mligo)
	@$(call test,mint_token.test.mligo)
	@$(call test,burn_token.test.mligo)
else
	@$(call test,$(SUITE).test.mligo)
endif

lint: ## lint code
	@npx eslint ./scripts --ext .ts

sandbox-start: ## start sandbox
	@./scripts/run-sandbox

sandbox-stop: ## stop sandbox
	@docker stop sandbox
