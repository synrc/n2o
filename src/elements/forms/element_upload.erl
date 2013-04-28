% vim: sw=4 ts=4 et
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include_lib ("wf.hrl").
-include_lib ("simple_bridge.hrl").
-compile(export_all).

%% #upload allows a user to upload a file.
%% 
%% How it works:
%% - This element creates an <input type=file ...> HTML element on the page, wrapped
%%   in a <form>, with all of the required parameters necessary to fake the system
%%   into believing it is a real postback call. 
%%
%% - When the user clicks the upload button, first the 'upload_started' event
%%   gets fired, calling start_upload_event(Tag) on the Module or Page.
%%
%% - Then, the browser begins uploading the file to the server. The multipart file
%%   is parsed in SimpleBridge.
%%
%% - Finally, once the upload is complete, control is passed on to Nitrogen, which reads 
%%   the parameters sent over in the first step and calls the 'upload_finished' event in
%%   this module.
%%
%% - The 'upload_finished' emits Javascript that causes *another* postback, this time
%%   to the 'upload_event' event in this module, which then calls 
%%   Module:finish_upload_event(Tag, OriginalName, TempFile, Node).
%%   The reason we do this extra postback is because the upload itself happens in a form
%%   separate from the main Nitrogen page (otherwise the main Nitrogen page would need to 
%%   refresh) so this is our way of getting the main page to see the event.


reflect() -> record_info(fields, upload).

render_element(Record) ->
    Anchor = Record#upload.anchor,
	Multiple = Record#upload.multiple,
    Droppable = Record#upload.droppable,
    DroppableText = Record#upload.droppable_text,
    FileInputText = Record#upload.file_text,
    ShowButton = Record#upload.show_button,
    ButtonText = Record#upload.button_text,
    StartedTag = {upload_started, Record},
    FinishedTag = {upload_finished, Record}, 
    FormID = wf:temp_id(),
    IFrameID = wf:temp_id(),
    ButtonID = wf:temp_id(),
    DropID = wf:temp_id(),
    DropListingID = wf:temp_id(),
    FileInputID = wf:temp_id(),
    FakeFileInputID = wf:temp_id(),

	Param = [
		{droppable,Droppable},
		{autoupload,not(ShowButton)}
	],

	JSONParam = nitro_mochijson2:encode({struct,Param}),
	SubmitJS = wf:f("Nitrogen.$send_pending_files(jQuery('#~s').get(0),jQuery('#~s').get(0));",[FormID,FileInputID]),
    UploadJS = wf:f("Nitrogen.$attach_upload_handle_dragdrop(jQuery('#~s').get(0),jQuery('#~s').get(0),~s);", [FormID,FileInputID,JSONParam]),

    PostbackInfo = wf_event:serialize_event_context(FinishedTag, Record#upload.id, undefined, ?MODULE),

    % Create a postback that is called when the user first starts the upload...
    wf:wire(Anchor, #event { show_if=(not ShowButton), type=change, delegate=?MODULE, postback=StartedTag }),
    wf:wire(ButtonID, #event { show_if=ShowButton, type=click, delegate=?MODULE, postback=StartedTag }),

    % If the button is invisible, then start uploading when the user selects a file.
    %wf:wire(Anchor, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
    wf:wire(ButtonID, #event { show_if=ShowButton, type=click, actions=SubmitJS }),

    wf:wire(UploadJS),

    % Set the dimensions of the file input element the same as
    % faked file input button has.
    wf:wire(wf:f("jQuery('#~s').width(jQuery('#~s').width()); jQuery('#~s').height(jQuery('#~s').height());",
        [FileInputID, FakeFileInputID, FileInputID, FakeFileInputID])),

    % Render the controls and hidden iframe...
    FormContent = [
        %% IE9 does not support the droppable option, so let's just hide the drop field
        "<!--[if lte IE 9]>
            <style type='text/css'> .upload_drop {display: none} </style>
        <![endif]-->",

        #panel{
            show_if=Droppable,
            id=DropID,
            class=[upload_drop,'dropzone-container'],
            body=[
                #panel{
                    class=[dropzone,'ui-corner-all'],
                    text=DroppableText
                }
            ]
        },
        #panel{
            %show_if=Droppable,
            class=upload_progress,
            body=""
        },
        #list{
            show_if=Droppable,
            id=DropListingID,
            class=upload_droplist
        },

%%  ORIGINAL!
%%        wf_tags:emit_tag(input, [
%%            {name, file},
%%            {multiple,Multiple},
%%            {class, [no_postback,FileInputID|Anchor]},
%%            {id, FileInputID},
%%            {type, file}
%%        ]),	
        #panel{
            style="position: relative;",
            body=[
                wf_tags:emit_tag(input, [
                    {type, button},
                    {style, "margin: 2px; border: 2px outset rgb(221, 221, 221); padding: 1px 6px; position: absolute; top: 0px; left: 0px; z-index: 1;"},
                    {value, FileInputText},
                    {id, FakeFileInputID}
                ]),

                wf_tags:emit_tag(input, [
                    {name, file},
                    {multiple, Multiple},
                    {class, [no_postback, FileInputID|Anchor]},
                    {id, FileInputID},
                    {type, file},
                    {style, "margin: 2px; border: 2px outset rgb(221, 221, 221); padding: 1px 6px; opacity: 0; filter:alpha(opacity: 0); position: relative; z-index: 2;"}
                ])
            ]
        },

        wf_tags:emit_tag(input, [
            {name, eventContext},
            {type, hidden},
            {class, no_postback},
            {value, PostbackInfo}
        ]),

        wf_tags:emit_tag(input, [
            {name, pageContext},
            {type, hidden},
            {class, no_postback},
            {value, ""}
        ]),

        wf_tags:emit_tag(input, [
            {type, hidden},
            {class, no_postback},
            {value, ""}
        ]),

        #button { id=ButtonID, show_if=ShowButton, text=ButtonText }
    ],

    [
        wf_tags:emit_tag(form, FormContent, [
            {id, FormID},
            {name, upload}, 
            {method, 'POST'},
            {enctype, "multipart/form-data"},
            {class, no_postback},
            {target, IFrameID}
        ])
    ].


% This event is fired when the user first clicks the upload button.
event({upload_started, Record}) ->
    Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
    Module:start_upload_event(Record#upload.tag);


% This event is called once the upload post happens behind the scenes.
% It happens somewhat outside of Nitrogen, so the next thing we do
% is trigger a postback that happens inside of Nitrogen. 
event({upload_finished, Record}) ->
    wf_context:type(first_request),
    Req = wf_context:request_bridge(),

    % % Create the postback...
    {Filename,NewTag} = case Req:post_files() of
        [] -> 
            {undefined,{upload_event, Record, undefined, undefined, undefined}};
        [#uploaded_file { original_name=OriginalName, temp_file=TempFile }|_] ->
            {OriginalName,{upload_event, Record, OriginalName, TempFile, node()}}
    end,

    % Make the tag...
    Anchor = wf_context:anchor(),
    ValidationGroup = wf_context:event_validation_group(),
    Postback = wf_event:generate_postback_script(NewTag, Anchor, ValidationGroup, ?MODULE, undefined),

    % Set the response...
    wf_context:data([
        "Nitrogen.$upload_finished(\"",wf:js_escape(Filename),"\");",
        Postback
    ]);

% This event is fired by the upload_finished event, it calls
% back to the page or control that contained the upload element.
event({upload_event, Record, OriginalName, TempFile, Node}) ->
    Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
    Module:finish_upload_event(Record#upload.tag, OriginalName, TempFile, Node).
