-module (element_htmlbox).
-author('doxtop@synrc.com').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").

render_element(R = #htmlbox{})->
  Id = case R#htmlbox.id of undefined-> wf:temp_id(); I -> I end,
  PreviewId = case R#htmlbox.post_target of undefined -> "preview_"++Id; T-> T end, 
  ToolbarId = wf:temp_id(),
  Html = case R#htmlbox.html of undefined -> ""; H -> wf:js_escape(H) end,
  Root = case R#htmlbox.root of undefined -> code:priv_dir(n2o); Path -> Path end,
  Up =  #upload{id = wf:temp_id(),
    root = Root,
    dir = R#htmlbox.dir,
    delegate = element_htmlbox,
    delegate_query = element_htmlbox,
    delegate_api = R#htmlbox.delegate_api,
    post_write = R#htmlbox.post_write,
    img_tool = R#htmlbox.img_tool,
    post_target = PreviewId,
    size = R#htmlbox.size},
  UploadPostback = wf_event:new(Up, Id, element_htmlbox, control_event, <<"{'msg': uid}">>),

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
        "statusbar: false,"++
        "plugins: 'paste, visualblocks, autolink',"++
        "visualblocks_default_state: false,"++
        "end_container_on_empty_block: true," ++
        "toolbar: 'pic, bq, cd'," ++
        "setup: function(ed){"++
          "ed.on('init', function(e){ $('#'+editorId).attr('tabIndex', 0); ed.setContent('~s'); });" ++
          "ed.on('GetContent', function(e){
            e.content=e.content.replace(/(<pre>(?:[^<](?!\\/pre))*<\\/pre>)/gi, function(a){return a.replace(/&amp;/g, '&');}); // :)
          });" ++
          "ed.addButton('pic', {title: 'image', onclick: function(e){
            var p = '~s';
            ed.execCommand('mceInsertContent', '~s', p + '<p></p>');
            wireUpload(Bert.encode(e.ui));
          }, icon: 'icon-picture' });" ++
          "ed.addButton('b', {title: 'bold', onclick: function(e){
            ed.execCommand('Bold');
          }, icon:'icon-bold', toggle: 'button'});" ++
          "ed.addButton('bq', {title: 'blockquote', onclick: function(e){
            ed.execCommand('formatBlock', false, 'blockquote');
          }, icon:'icon-quote-right'});" ++
          "ed.addButton('cd', {title: 'code', onclick: function(e){
            ed.execCommand('formatBlock', false, 'pre');
          }, icon:'icon-code'});" ++
          "ed.addButton('blks', {title: 'show blocks', class:'pull-right', onclick: function(e){
            ed.execCommand('mceVisualBlocks');
          }, icon:'icon-eye-open', toggle: 'button'});" ++

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
control_event(Cid, {query_file, Root, Dir, File, MimeType, PostWrite, Target})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      wf:wire(wf:f("$('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();", [Cid, filename:join([Dir, Name])])),

      ThDir = filename:join([Root, Dir, "thumbnail"]),
      post_write(PostWrite, Target, Root, Dir, Name, MimeType, filename:join([ThDir--Root, Name])),

      FileInfo#file_info.size;
    {error, _} -> 0 end,
  {exist, Size};
control_event(Cid, {Root, Dir, File, MimeType, Data, ActionHolder, PostWrite, ImgTool, Target, Size}) ->
    Full = filename:join([Root, Dir, File]),
    file:write_file(Full, Data, [write, raw]),
    wf:wire(wf:f("$('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();", [Cid, filename:join([Dir, File])])),

    case PostWrite of undefined-> undefined;
    Api ->
        Thumb = case ImgTool of undefined ->"";
        M ->
            Ext = filename:extension(File),
            Name = filename:basename(File, Ext),
            ThDir = filename:join([Root, Dir, "thumbnail"]),
            [begin
                Th = filename:join([ThDir, Name++"_"++integer_to_list(X)++"x"++integer_to_list(Y)++Ext]),
                En = filelib:ensure_dir(Th),
                M:make_thumb(Full, X, Y, Th) end || {X, Y}<- Size],
                filename:join([ThDir--Root, Name++Ext]) end,
            post_write(Api, Target, Root, Dir, File, MimeType, Thumb) end,
    wf:flush(ActionHolder).

post_write(undefined,_,_,_,_,_,_) -> skip;
post_write(Api, Target, Root, Dir, File, MimeType, Thumb)->
    Full = filename:join([Root, Dir, File]),
    wf:wire(wf:f("~s({preview: '~s', id:'~s', file:'~s', type:'~s', thumb:'~s'});", [
        Api, Target, element_upload:hash(Full), filename:join([Dir,File]), MimeType, Thumb ])).
