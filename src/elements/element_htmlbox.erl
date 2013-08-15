-module (element_htmlbox).
-author('doxtop@synrc.com').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").

render_element(R = #htmlbox{})->
  Id = case R#htmlbox.id of undefined-> wf:temp_id(); I -> I end,
  PreviewId = case R#htmlbox.post_target of undefined -> "preview_"++Id; T-> T end, 
  ToolbarId = wf:temp_id(),
  Html = R#htmlbox.html,
  Root = case R#htmlbox.root of undefined -> code:priv_dir(n2o); Path -> Path end,
  Up =  #upload{id=wf:temp_id(), dir=R#htmlbox.dir, delegate=element_htmlbox, delegate_query=element_htmlbox, root=Root, post_write=R#htmlbox.post_write, img_tool=R#htmlbox.img_tool, post_target=PreviewId, size=R#htmlbox.size},
  UploadPostback = wf_event:generate_postback_script(Up, ignore, Id, element_htmlbox, control_event, <<"{'msg': uid}">>),

  wf:wire(wf:f(
    "$(function(){"++
      "function wireUpload(uid){~s;};"++
      "var editorId = '~s';" ++ 
      "$('#'+editorId).tinymce({" ++
        "inline: true," ++
        "selector: \"div#\"+editorId," ++
        "script_url: '~s',"++
        "theme: 'n2o'," ++
        "theme_modern_toolbar_location: \"external\"," ++
        "fixed_toolbar_container: '#'+'~s', "++
        "paste_as_text: true,"++
        "menubar: false,"++
        "statusbar: false,"
        "plugins: 'paste',"++
        "toolbar: 'ins'," ++
        "setup: function(ed){"++
          "ed.on('init', function(e){ $('#'+editorId).attr('tabIndex', 0); ed.setContent('~s'); });" ++
          "ed.addButton('ins', {title: 'image', onclick: function(e){
            var p = '~s';
            ed.execCommand('mceInsertContent', '~s', p + '<p></p>');
            wireUpload(Bert.encode(e.ui));
          }, icon: 'icon-picture' });" ++
      "}" ++
    "});"++
  "});", [UploadPostback, Id, R#htmlbox.script_url, ToolbarId, Html, element_upload:render(Up), Up#upload.id])),

  case R#htmlbox.toolbar_script of undefined -> []; Script -> wf:wire(wf:f("~s", [Script])) end,

  P = #panel{class=["htmlbox-container"], body=[
      #panel{id=ToolbarId, class= [span12, case R#htmlbox.toolbar_class of undefined -> []; C -> C end], body= <<"">>},
      #panel{id=Id, class=[span12], tabindex = 0}
  ]},
  element_panel:render_element(P).

control_event(_Cid, #upload{} = Tag) -> element_upload:wire(Tag);
control_event(Cid, {query_file, Root, Dir, File, MimeType})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      wf:wire(wf:f("$('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();", [Cid, filename:join([Dir, Name])])),
      FileInfo#file_info.size;
    {error, _} -> 0 end,
  {exist, Size};
control_event(Cid, {Root, Dir, File, MimeType, Data, ActionHolder, PostWrite, ImgTool, Target, Size}) ->
  Full = filename:join([Root, Dir, File]),

  file:write_file(Full, Data, [write, raw]),
  wf:wire(wf:f("$('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();", [Cid, filename:join([Dir, File])])),

  case PostWrite of
    undefined-> undefined;
    Api ->
      Thumb = case ImgTool of
        undefined ->"";
        M ->
          Ext = filename:extension(File),
          Name = filename:basename(File, Ext),
          ThDir = filename:join([Root, Dir, "thumbnail"]),
          filelib:ensure_dir(ThDir),
          [begin
            Th = filename:join([ThDir, Name++"_"++integer_to_list(X)++"x"++integer_to_list(Y)++Ext]),
            M:make_thumb(Full, X, Y, Th) end || {X, Y}<- Size],
          filename:join([ThDir--Root, Name++Ext])
      end,
      wf:wire(wf:f("~s({preview: '~s', id:'~s', file:'~s', type:'~s', thumb:'~s'});", [Api, Target, element_upload:hash(Full), filename:join([Dir,File]), MimeType, Thumb]))
  end,
  wf:flush(ActionHolder).