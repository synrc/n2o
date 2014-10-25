-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

title() -> [ <<"Login">> ].
main() -> #dtl{file = "login", app=n2o_sample,bindings=[{title,title()},{body,body()}]}.

body() ->
 [ #span{id=display}, #br{},
            #span{body="Login: "}, #textbox{id=user,autofocus=true}, #br{},
            #span{body="Password: "}, #password{id=pass},
            #button{body="Login",postback=login,source=[user,pass]} ].

event(init) -> js_session:ensure_sid([],?CTX);

event(login) ->
    User = wf:q(user),
    wf:update(display,User),
    wf:user(User),
    wf:redirect("/index"),
    ok;

event(_) -> [].
