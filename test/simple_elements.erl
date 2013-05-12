-module(simple_elements).
-include_lib("n2o/include/wf.hrl").
-include_lib("common_test/include/ct.hrl").
-include("test.hrl").
-compile(export_all).

main() -> #template{ file = ?TEMPLATE }.
title() -> "N2O Test".
body() ->
    [
     #label{text = "test label"},
     #textbox{text = "test textbox"}
    ].
