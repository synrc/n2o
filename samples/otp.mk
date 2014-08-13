VM       := rels/web/files/vm.args
SYS      := rels/web/files/sys.config
PLT_NAME := ~/.n2o_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR  ?= rels/web/devbox
LOG_DIR  ?= rels/web/devbox/logs
empty    :=
ROOTS    := apps deps
space    := $(empty) $(empty)
comma    := $(empty),$(empty)
VSN      := $(shell git rev-parse HEAD | head -c 6)
DATE     := $(shell date "+%Y%m%d-%H%M%S")
ERL_LIBS := $(subst $(space),:,$(ROOTS))
relx     := "{release,{$(RELEASE),\"$(VER)\"},[$(RELEASE)]}.\\n{include_erts,true}.\
\\n{extended_start_script,true}.\\n{generate_start_script,true}.\\n{sys_config,\"$(SYS)\"}.\
\\n{vm_args,\"$(VM)\"}.\\n{overlay,[{mkdir,\"log/sasl\"}]}."

test: eunit ct
compile: get-deps
delete-deps get-deps compile update-deps:
	./mad $@
clean:
	rm -f .applist
	rebar $@
.applist:
	$(eval APPS := $(subst deps/,,$(subst apps/,,$(shell find apps deps -maxdepth 1 -mindepth 1 -type d))))
	./orderapps.erl $(APPS) > $@
$(RUN_DIR) $(LOG_DIR):
	mkdir -p $(RUN_DIR) & mkdir -p $(LOG_DIR)
console: .applist
	ERL_LIBS=$(ERL_LIBS) erl +pc unicode $(ERL_ARGS) -eval '[application:start(A) || A <- $(shell cat .applist)]'
start: $(RUN_DIR) $(LOG_DIR) .applist
	RUN_ERL_LOG_GENERATIONS=1000 RUN_ERL_LOG_MAXSIZE=20000000 \
	ERL_LIBS=$(ERL_LIBS) run_erl -daemon $(RUN_DIR)/ $(LOG_DIR)/ "exec $(MAKE) console"
attach:
	to_erl $(RUN_DIR)/
release:
	echo $(relx) > relx.config && relx
stop:
	@kill -9 $(shell ps ax -o pid= -o command=|grep $(RELEASE)|grep $(COOKIE)|awk '{print $$1}')
$(PLT_NAME):
	$(eval APPS := $(subst deps/,,$(subst apps/,,$(shell find apps deps -maxdepth 1 -mindepth 1 -type d))))
	ERL_LIBS=$(ERL_LIBS) dialyzer --build_plt --output_plt $(PLT_NAME) --apps $(APPS) || true
dialyze: $(PLT_NAME) compile
	$(eval APPS := $(shell find apps deps -maxdepth 1 -mindepth 1 -type d))
	@$(foreach var,$(APPS),(echo "Process $(var)"; dialyzer -q $(var)/ebin --plt $(PLT_NAME) --no_native -Werror_handling -Wunderspecs -Wrace_conditions -Wno_undefined_callbacks);)
tar: release
	tar zcvf $(RELEASE)-$(VSN)-$(DATE).tar.gz _rel/lib/*/ebin _rel/lib/*/priv _rel/bin _rel/releases
eunit:
	rebar eunit skip_deps=true
ct:
	rebar ct skip_deps=true verbose=1

.PHONY: delete-deps get-deps compile clean console start attach release update-deps dialyze ct eunit tar
