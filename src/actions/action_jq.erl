-module(action_jq).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    Target = Record#jq.target,
    Methods = Record#jq.method,
    Arg = string:join([wf:to_list(A)||A<-Record#jq.args],","),
    string:join([wf:f("$('#~s').~s(~s);", [Target,Method,Arg]) || Method <- Methods],"").
