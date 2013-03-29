#!/bin/bash

FILES=apps/web/priv/static/nitrogen
rm -rf $FILES
ln -s ../../../../deps/nitrogen_core/www $FILES
