-module(element_email_link).
-compile('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, email_link).

render_element(Rec = #email_link{}) -> 
    Email = Rec#email_link.email,
    Text = if
        Rec#email_link.body == [] ->
            wf:html_encode(Email);
        true -> Rec#email_link.body
    end,

    #link{
        id=Rec#email_link.id,
        class=Rec#email_link.class,
        title=Rec#email_link.title,
        body=Text,
        new=false,
        url=["mailto:",Email]
    }.
