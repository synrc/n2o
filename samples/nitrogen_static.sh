#!/bin/bash

FILES=apps/web/priv/static/nitrogen
rm -rf $FILES
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
