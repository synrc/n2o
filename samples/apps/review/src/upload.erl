-module(upload).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").

-record(upload, {?CTRL_BASE(upload), name, value}).

render_element(#upload{id=Id} = U) ->
    Uid = case Id of undefined -> wf:temp_id(); I -> I end,
    wf:wire(select()),
    wf:wire(browse()),
    wf:wire(start()),
    wf:wire(stop()),
    Upload = #panel  { body = [
             #input  { id   = Uid,         type    = <<"file">>, style = "display:none" },
             #span   { id   = ftp_status,  body    = [] },
             #span   { body = [
             #button { id   = ftp_open,    body = "Browse" },
             #button { id   = ftp_start,   body = "Upload" },
             #button { id   = ftp_stop,    body = "Stop" }
    ] } ] }, wf:render(Upload).

browse() -> "{ var x=qi('ftp_open');  if (x) x.addEventListener('click', function(e) { qi('upload').click(); e.preventDefault(); }); }".
start()  -> "{ var x=qi('ftp_start'); if (x) x.addEventListener('click', function(e) { ftp.start(); }); }".
stop()   -> "{ var x=qi('ftp_stop');  if (x) x.addEventListener('click', function(e) { ftp.stop(); }); }".
select() -> "{ var x=qi('upload');    if (x) x.addEventListener('change',function(e) { ftp.init(this.files[0],1); }); }".
