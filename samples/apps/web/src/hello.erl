-module(hello).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main2() -> [ <<"N2O">> ].

main() ->
%    Title = "Title",
%    Body = "Body",
    Title = wf:render(title()),
    Body = wf:render(body()),
    [ #dtl{file = "hello", bindings=[{title,Title},{hello,Body}]} ].

title() -> [ <<"N2O">> ].

body() -> %% area of http handler
  [
    #button{id=replace,body= <<"Goto Index">>,postback=replace},
    #panel { id=ok, body = io_lib:format("'/hello?x=' is ~p",[wf:qs(<<"x">>)]) }, #br{},
    #panel { id=n2ostatus }
 ].

event(init) -> [];

event(replace) ->
    wf:wire(#redirect{url="index",nodrop=false}).

