-module(action_confirm).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) -> 
    Control = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    Postback = Record#confirm.postback,
    PostbackScript = wf_event:new(Postback, Control, Delegate, event, "[]", []),
    [
        wf:f("if (confirm(\"~s\")) {", [wf:js_escape(Record#confirm.text)]),
        PostbackScript,
        "}"
    ].

