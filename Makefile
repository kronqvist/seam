DOCKER_IMAGE=$(notdir $(CURDIR))
GROUP_ID=$(shell id -g)
REBAR_CACHE_DIR=$(CURDIR)/.cache
USERNAME=$(shell whoami)
USER_ID=$(shell id -u)

DOCKER_RUN_OPTS = \
    -e REBAR_CACHE_DIR=$(REBAR_CACHE_DIR) \
    -e SSH_AUTH_SOCK=/ssh-agent \
    -h $(notdir $(CURDIR)) \
    -u $(USER_ID):$(GROUP_ID) \
    -v $(CURDIR):$(CURDIR) \
    -v $(CURDIR)/.git \
    -v $(HOME)/.ssh/:$(HOME)/.ssh:ro \
    -v $(SSH_AUTH_SOCK):/ssh-agent \
    -w $(CURDIR) \
    --rm

PORT_BIN=priv/lexberl
PORT_SRC=csrc/lexberl.c

GEMINI_API_KEY_FILE=$(HOME)/.gemini_api_key
OPENAI_API_KEY_FILE=$(HOME)/.openai_api_key
X11_OPTS = -e DISPLAY=$(DISPLAY) -v /tmp/.X11-unix:/tmp/.X11-unix

.PHONY: all bash build clean dialyzer doc image shell test gemini

all: build

build:
	mkdir -p priv
	docker run $(DOCKER_RUN_OPTS) $(DOCKER_IMAGE) rebar3 compile

clean:
	docker run $(DOCKER_RUN_OPTS) $(DOCKER_IMAGE) rebar3 clean

doc:
	docker run $(DOCKER_RUN_OPTS) $(DOCKER_IMAGE) rebar3 edoc

image:
	docker build --build-arg USER_ID=$(USER_ID) --build-arg GROUP_ID=$(GROUP_ID) --build-arg USERNAME=$(USERNAME) -t $(DOCKER_IMAGE) .

shell:
	docker run $(DOCKER_RUN_OPTS) --env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) $(X11_OPTS) -ti $(DOCKER_IMAGE) rebar3 as shell shell

bash:
	docker run $(DOCKER_RUN_OPTS) \
		--env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) \
		$(X11_OPTS) \
		-ti \
		$(DOCKER_IMAGE) \
		bash

gemini:
	docker run $(DOCKER_RUN_OPTS) \
		--env GEMINI_API_KEY=$$(cat $(GEMINI_API_KEY_FILE)) \
		--env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) \
		-v $(HOME)/.gemini:$(HOME)/.gemini \
		$(X11_OPTS) \
		-it \
		$(DOCKER_IMAGE) \
		gemini

codex:
	docker run $(DOCKER_RUN_OPTS) \
		--env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) \
		-it \
		$(DOCKER_IMAGE) \
		codex

CLAUDE_ARGS = --dangerously-skip-permissions
ifdef RESUME
CLAUDE_ARGS += --resume $(RESUME)
endif

claude:
	docker run $(DOCKER_RUN_OPTS) \
		--env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) \
		--env CLAUDE_CODE_DISABLE_AUTO_MEMORY=0 \
		-v $(HOME)/.claude:$(HOME)/.claude \
		-v $(HOME)/.claude.json:$(HOME)/.claude.json \
		-it \
		$(DOCKER_IMAGE) \
		claude $(CLAUDE_ARGS)

dialyzer:
	docker run $(DOCKER_RUN_OPTS) $(DOCKER_IMAGE) rebar3 dialyzer

test:
	docker run $(DOCKER_RUN_OPTS) \
		--env OPENAI_API_KEY=$$(cat $(OPENAI_API_KEY_FILE)) \
		$(DOCKER_IMAGE) rebar3 eunit

smoke:
	docker run $(DOCKER_RUN_OPTS) $(DOCKER_IMAGE) rebar3 eunit -m lattice_smoke_test
