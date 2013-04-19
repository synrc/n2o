#!/bin/bash

FILES=apps/web/priv/static/nitrogen
rm -rf $FILES
ln -s ../../../../apps/n2o/priv/static/n2o $FILES
