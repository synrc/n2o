-module(elements).
-include_lib("n2o/include/wf.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

main() -> 
    Title = wf_render_elements:render_elements(title()),
    Body = wf_render_elements:render_elements(body()),
    #dtl{file = "index.html", app=n2o, folder = "test", bindings=[{title,Title},{body,Body}]}.

title() -> "N2O Test".
body() ->
    [
     #label{body = "test label"},
     #textbox{body = "test textbox"}
    ].
