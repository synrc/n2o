#!/bin/bash

FILES=deps/n2o_sample/priv/static/nitrogen
rm -rf $FILES
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
