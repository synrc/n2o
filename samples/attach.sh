#!/bin/sh

NODE=${1:-"web"}
BIN="rels/$NODE/node/bin/node"

$BIN attach

