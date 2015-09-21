-module(upload).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").

-record(upload, {?CTRL_BASE(upload), name, value}).

render_element(#upload{id=Id} = U) ->
    Uid = case Id of undefined -> wf:temp_id(); I -> I end,
    bind(ftp_open,  click,  "qi('upload').click(); e.preventDefault();"),
    bind(ftp_start, click,  "ftp.start();"),
    bind(ftp_stop,  click,  "ftp.stop();"),
    bind(nitro:to_atom(Uid), change, "ftp.init(this.files[0],1);"),
    Upload = #panel  { body = [
             #input  { id   = Uid,         type    = <<"file">>, style = "display:none" },
             #span   { id   = ftp_status,  body    = [] },
             #span   { body = [
             #button { id   = ftp_open,    body = "Browse" },
             #button { id   = ftp_start,   body = "Upload" },
             #button { id   = ftp_stop,    body = "Stop" }
    ] } ] }, wf:render(Upload).

bind(Control,Event,Code) ->
    wf:wire(#bind{target=Control,type=Event,postback=Code}).
