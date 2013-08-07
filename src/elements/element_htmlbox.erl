-module (element_htmlbox).
-author('doxtop@synrc.com').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

render_element(R = #htmlbox{})->
  Id = case R#htmlbox.id of undefined-> wf:temp_id(); I -> I end,
  PreviewId = case R#htmlbox.post_target of undefined -> "preview_"++Id; T-> T end, 
  ToolbarId = wf:temp_id(),
  Html = R#htmlbox.html,
  Root = case R#htmlbox.root of undefined -> code:priv_dir(n2o); Path -> Path end,
  Up =  #upload{id=wf:temp_id(), dir=R#htmlbox.dir, delegate=element_htmlbox, root=Root, post_write=R#htmlbox.post_write, img_tool=R#htmlbox.img_tool, post_target=PreviewId},
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
          "ed.addButton('ins', {title: 'image', onclick: function(){
            var p = '~s';
            ed.execCommand('mceInsertContent', '~s', p + '<p></p>');
          }, icon: 'icon-picture' });" ++
          "ed.on('ExecCommand', function(e){
            if(e.command == 'mceInsertContent'){ wireUpload(Bert.binary(e.ui)); }
          });" ++
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
control_event(Cid, {Root, Dir, File, MimeType, Data, ActionHolder, PostWrite, ImgTool, Target}) ->
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
          Th = filename:join([Root, Dir, "thumbnail", Name++"_thumb"++Ext]),
          filelib:ensure_dir(Th),
          M:make_thumb(Full, 200, 120, Th),
          Th
      end,
      wf:wire(wf:f("~s({preview: '~s', id:'~s', file:'~s', type:'~s', thumb:'~s'});", [Api, Target, element_upload:hash(Full), filename:join([Dir,File]), MimeType, [Thumb--Root]]))
  end,
  wf:flush(ActionHolder).