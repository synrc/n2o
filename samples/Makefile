COOKIE := node_runner
RELEASE := n2o_sample
VER := 1.0.0
APP := apps/n2o_sample/priv/static/nitrogen

default: get-deps compile static-link
static-link:
	rm -rf $(APP)
	ln -s ../../../../deps/n2o/priv $(APP)

include otp.mk
