RELEASE := n2o_sample
COOKIE  := node_runner
APPS    := kernel stdlib sasl sync gproc cowlib cowboy ranch erlydtl n2o n2o_sample
VER     := 1.0.0
VM      := rels/web/files/vm.args
SYS     := rels/web/files/sys.config
PLT_NAME := ~/.n2o_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR ?= rels/web/devbox
LOG_DIR ?= rels/web/devbox/logs
APP     := apps/n2o_sample/priv/static/nitrogen

default: get-deps compile static-link
static-link:
	rm -rf $(APP)
	ln -s ../../../../deps/n2o_scripts/n2o $(APP)

include otp.mk
