#!/bin/bash

N2O=deps/n2o/priv/static
APP=deps/n2o_sample/priv/static/nitrogen
XEN=apps/n2o_sample/priv
rm -rf $N2O
rm -rf $APP
rm -rf $XEN
mkdir -p apps/n2o_sample
ln -s ../../n2o_scripts $N2O
ln -s ../../../../deps/n2o/priv/static/n2o $APP
ln -s ../../deps/n2o_sample/priv $XEN
