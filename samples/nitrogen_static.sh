#!/bin/bash

HAPPYXEN=apps/n2o_sample/priv
FILES=deps/n2o_sample/priv/static/nitrogen
rm -rf $FILES
rm -rf $HAPPYXEN
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
ln -s ../../deps/n2o_sample/priv $HAPPYXEN
