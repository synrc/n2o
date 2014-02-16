RELEASE := skyline
COOKIE  := sample
APPS    := kernel stdlib sasl sync n2o_sample
VER     := 1.0.0
VM      := rels/web/files/vm.args
SYS     := rels/web/files/sys.config
PLT_NAME := ~/.n2o_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR ?= rels/web/devbox
LOG_DIR ?= rels/web/devbox/logs
N2O     := deps/n2o/priv/static
APP     := deps/n2o_sample/priv/static/nitrogen
XEN     := apps/n2o_sample/priv

default: compile static-link
static-link:
	rm -rf $(N2O)
	rm -rf $(APP)
	rm -rf $(XEN)
	mkdir -p apps/n2o_sample
	ln -s ../../n2o_scripts $(N2O)
	ln -s ../../../../deps/n2o/priv/static/n2o $(APP)
	ln -s ../../deps/n2o_sample/priv $(XEN)

include otp.mk
