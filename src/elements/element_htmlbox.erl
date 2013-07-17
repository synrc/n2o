-module (element_htmlbox).
-author('doxtop@synrc.com').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

render_element(R = #htmlbox{})->
  Id = R#htmlbox.id,
  ToolbarId = wf:temp_id(),
  Html = R#htmlbox.html,
  % tinymce based 
  wf:wire(wf:f(
    "$(function(){"++
      "var editorId = '~s';" ++ 
      "$('#'+editorId).tinymce({" ++
        "inline: true," ++
        "selector: \"div#\"+editorId," ++
        "script_url: '~s',"++
        "theme: 'modern'," ++
        "theme_modern_toolbar_location: \"external\"," ++
        "fixed_toolbar_container: '#'+'~s', "++
        "menubar: false,"++
        "statusbar: false,"
        "plugins: 'link image',"++
        "toolbar: 'image'," ++
        "setup: function(ed){"++
          "ed.on('init', function(e){$('#'+editorId).attr('tabIndex', 0); ed.setContent('~s') });" ++
          "ed.on('SaveContent', function(e){});"  ++
%        "$(ed).unbind('SaveContent');
%         encodeURIComponent($.trim(ed.getContent().replace(/[\\s\\n\\r]+/g, ' ')));
      "}" ++
    "});"++
  "});", [Id, R#htmlbox.script_url, ToolbarId, Html])),

  P = #panel{class=["row-fluid"], body=[
    #panel{id=Id, class=[span12], tabindex = 0},
    #panel{id=ToolbarId, style="display:none"}
  ]},
  element_panel:render_element(P).
