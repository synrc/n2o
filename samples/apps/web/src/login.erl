-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

title() -> [ <<"Login">> ].

main() ->
    Title = wf_render_elements:render_elements(title()),
    Body = wf_render_elements:render_elements(body()),
  [ #dtl{file = "login", bindings=[{title,Title},{body,Body}]} ].

body() -> [ #span{id=display}, #br{},
            #span{text="Login: "}, #textbox{id=user}, #br{},
            #span{text="Password: "}, #password{id=pass},
            #button{text="Login",postback=login,source=[user,pass]} ].

event(init) -> [];

event(login) -> User = wf:q(user), wf:update(display,User), wf:user(User), wf:redirect("index").
