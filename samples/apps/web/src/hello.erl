-module(hello).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

%main() -> [ <<"N2O">> ].

main() ->
%    Title = "Title",
%    Body = "Body",
    Title = wf_render_elements:render_elements(title()),
    Body = wf_render_elements:render_elements(body()),
    [ #dtl{file = "hello", bindings=[{title,Title},{hello,Body}]} ].

title() -> [ <<"N2O">> ].

body() -> %% area of http handler
  [
    #button{id=replace,text= <<"Goto Index">>,postback=replace},
    #panel { id=ok, text = io_lib:format("'/hello?x=' is ~p",[wf:qs(<<"x">>)]) }, #br{},
    #panel { id=n2ostatus }
 ].

event(init) -> [];

event(replace) ->
    wf:wire(#redirect{url="index",nodrop=false}).

