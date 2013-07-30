-module (element_htmlbox).
-author('doxtop@synrc.com').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

render_element(R = #htmlbox{})->
  Id = R#htmlbox.id,
  ToolbarId = wf:temp_id(),
  Html = R#htmlbox.html,
  Up =  #upload{id=wf:temp_id(), delegate=element_htmlbox, root=code:priv_dir(web)++"/static"},
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

    "$('.htmlbox-toolbar').scrollToFixed({
      marginTop: function(){return $('.navbar-fixed-top').height();},
      limit: function(){ var ed = $('#'+editorId); return ed.offset().top + ed.height() - $(this).height() + 10; },
      preUnfixed: function(){ $(this).parent().css('overflow','inherit'); },
      preAbsolute:function(){ $(this).parent().css('overflow', 'hidden'); }
    });"++

  "});", [UploadPostback, Id, R#htmlbox.script_url, ToolbarId, Html, element_upload:render(Up), Up#upload.id])),

  P = #panel{class=["htmlbox-container"], body=[
      #panel{id=ToolbarId, class= ["htmlbox-toolbar"], body= <<"">>},
      #panel{id=Id, class=[span12], tabindex = 0}
  ]},
  element_panel:render_element(P).

control_event(Cid, #upload{} = Tag) -> element_upload:wire(Tag);
control_event(Cid, {File, MimeType, Data, ActionHolder}) ->
  Base = code:priv_dir(web),
  file:write_file(File, Data, [write, raw]),
  wf:wire(wf:f("$('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();", [Cid, File--Base])),
  wf:flush(ActionHolder),
  ok.

