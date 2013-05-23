-ifndef(wf_inc).
-define(wf_inc, ok).

-record(handler, {name, module, config, state}).
-record(context, {handlers, actions, req, module, path, session, params}).
-record(ev,      {module, payload, trigger, name :: api_event | control_event | event | atom() }).

%%% LOGGING %%%
-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG, error_logger:info_msg("DEBUG: ~p:~p~n", [?MODULE, ?LINE])).
-endif.

%%% GUARDS %%%
-define(IS_STRING(Term), (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

%%% TERNARY IF AND VARIATIONS %%%
-define(WF_IF(Term,IfTrue,IfFalse),
    case (Term) of
        F when F==false;F==undefined;F==[] -> IfFalse;
        _ -> IfTrue
    end).

-define(WF_IF(Term,IfTrue), ?WF_IF(Term,IfTrue,"")).


%%% FRAMEWORK %%%

%%% Elements %%%
-define(ELEMENT_BASE(Module), is_element=is_element, module=Module, id, anchor, actions, show_if=true, class="", style="", source=[]).
-record(elementbase, {?ELEMENT_BASE(undefined)}).
-record(template, {?ELEMENT_BASE(element_template), file, bindings=[] }).
-record(dtl, {?ELEMENT_BASE(element_dtl), file="index", bindings=[], app=web, folder="priv/templates" }).
-record(function_el, {?ELEMENT_BASE(element_function), function=fun() -> [] end}).
-record(body, {?ELEMENT_BASE(element_body), title="", body=[]}).
-record(h1, {?ELEMENT_BASE(element_h1), text="", html_encode=true}).
-record(h2, {?ELEMENT_BASE(element_h2), text="", html_encode=true}).
-record(h3, {?ELEMENT_BASE(element_h3), text="", html_encode=true}).
-record(h4, {?ELEMENT_BASE(element_h4), text="", html_encode=true}).
-record(h5, {?ELEMENT_BASE(element_h5), text="", html_encode=true}).
-record(h6, {?ELEMENT_BASE(element_h6), text="", html_encode=true}).
-record(list, {?ELEMENT_BASE(element_list), numbered=false, body=[], data_fields=[]}).
-record(li, {?ELEMENT_BASE(element_li), body=[], text="", html_encode=true, role="", data_fields=[] }).
-record(br, {?ELEMENT_BASE(element_br) }).
-record(hr, {?ELEMENT_BASE(element_hr) }).
-record(p, {?ELEMENT_BASE(element_p), body="", text="", html_encode=true}).
-record(i, {?ELEMENT_BASE(element_i), text="", html_encode=true}).
-record(b, {?ELEMENT_BASE(element_b), text="", html_encode=true}).
-record(label, {?ELEMENT_BASE(element_label), body="", text="", html_encode=true, for=""}).
-record(pre, {?ELEMENT_BASE(element_pre), text="", html_encode=true}).
-record(strong, {?ELEMENT_BASE(element_strong), body="", text="", html_encode=true}).
-record(em, {?ELEMENT_BASE(element_em), body="", text="", html_encode=true}).
-record(value, {?ELEMENT_BASE(element_value), text="", html_encode=true}).
-record(link, {?ELEMENT_BASE(element_link), title = "", text="", body="", new=false, html_encode=true, mobile_target=false, mobile_dialog=false, data_fields=[], url="javascript:", postback, delegate, name}).
-record(email_link, {?ELEMENT_BASE(element_email_link), title="",text="",body="",html_encode=true,email=""}).
-record(error, {?ELEMENT_BASE(element_error), text="", html_encode=true}).
-record(span, {?ELEMENT_BASE(element_span), body="", text="", html_encode=true}).
-record(button, {?ELEMENT_BASE(element_button), text="Button", html_encode=true, click, postback, delegate}).
-record(literal, {?ELEMENT_BASE(element_literal), text="", html_encode=true}).
-record(textbox, {?ELEMENT_BASE(element_textbox), text="", maxlength="", placeholder="", html_encode=true, next, postback, delegate, html_name}).
-record(hidden, {?ELEMENT_BASE(element_hidden), text="", html_encode=true, html_name, disabled=false}).
-record(textarea, {?ELEMENT_BASE(element_textarea), text="", placeholder="", html_encode=true, html_name}).
-record(range, {?ELEMENT_BASE(element_range), data_fields=[], min=0, max=100, step=1, value=0, next, postback, delegate}).
-record(datepicker_textbox, {?ELEMENT_BASE(element_datepicker_textbox), text="", next, html_encode=true, validators=[], options = [{dateFormat, "yy-mm-dd"}] }).
-record(dropdown, {?ELEMENT_BASE(element_dropdown), options=[], html_encode=true, postback, delegate, value, multiple=false, disabled=false, data_fields=[], html_name}).
-record(option, { text="", value=undefined, selected=false, show_if=true }).
-record(checkbox, {?ELEMENT_BASE(element_checkbox), text="", html_encode=true, checked=false, value="on", postback, delegate, html_name}).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup), body=[]}).
-record(radio, {?ELEMENT_BASE(element_radio), text="", html_encode=true, value, name, checked=false, postback, delegate, html_name}).
-record(password, {?ELEMENT_BASE(element_password), text="", html_encode=true, next, postback, delegate, html_name}).
-record(restful_form, {?ELEMENT_BASE(element_restful_form), method="POST", action, html_name, enctype, body=[]}).
-record(restful_submit, {?ELEMENT_BASE(element_restful_submit), text="Submit", html_encode=true, html_name}).
-record(restful_reset, {?ELEMENT_BASE(element_restful_reset), text="Cancel", html_encode=true, html_name}).
-record(restful_upload, {?ELEMENT_BASE(element_restful_upload), html_encode=true, html_name}).
-record(panel, {?ELEMENT_BASE(element_panel), body="", text="", html_encode=true, data_fields=[]}).
-record(fieldset, {?ELEMENT_BASE(element_fieldset), body="", text="", html_encode=true, legend_body="", legend_text="", legend_html_encode=true}).
-record(spinner, {?ELEMENT_BASE(element_spinner), image="/nitrogen/spinner.gif"}).
-record(image, {?ELEMENT_BASE(element_image), image="", alt}).
-record(lightbox, {?ELEMENT_BASE(element_lightbox), body="" }).
-record(table, {?ELEMENT_BASE(element_table), rows, header=[], footer=[]}).
-record(tablerow, {?ELEMENT_BASE(element_tablerow), cells}).
-record(tableheader, {?ELEMENT_BASE(element_tableheader), text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(tablecell, {?ELEMENT_BASE(element_tablecell), text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(singlerow, {?ELEMENT_BASE(element_singlerow), cells}).
-record(file, {?ELEMENT_BASE(element_file), file}).
-record(flash, {?ELEMENT_BASE(element_flash)}).
-record(placeholder, {?ELEMENT_BASE(element_placeholder), body=[]}).
-record(bind, {?ELEMENT_BASE(element_bind), data=[], map=[], transform, acc=[], body=[], empty_body=[]}).
-record(sortblock, {?ELEMENT_BASE(element_sortblock), tag, items=[], group, connect_with_groups=none, handle, placeholder="", force_placeholder_size=false, delegate=undefined }).
-record(sortitem, {?ELEMENT_BASE(element_sortitem), tag, body=[] }).
-record(draggable, {?ELEMENT_BASE(element_draggable), tag, body=[], group, handle, clone=true, revert=true, scroll=true, container = false, zindex = false}).
-record(droppable, {?ELEMENT_BASE(element_droppable), tag, body=[], accept_groups=all, active_class=active, hover_class=hover, delegate=undefined}).
-record(gravatar, {?ELEMENT_BASE(element_gravatar), email="", size="80", rating="g", default=""}).

-record(inplace_textarea, {?ELEMENT_BASE(element_inplace_textarea), tag, text="", html_encode=true, start_mode=view, validators=[], delegate=undefined}).
-record(inplace_textbox, {?ELEMENT_BASE(element_inplace_textbox), tag, text="", html_encode=true, start_mode=view, validators=[], delegate=undefined}).
-record(inplace, {?ELEMENT_BASE(element_inplace), tag, text="", delegate=undefined, view, edit, start_mode=view}).

-record(upload, {?ELEMENT_BASE(element_upload), delegate, tag, show_button=true, file_text="Select file", button_text="Upload", droppable=false, droppable_text="Drop Files Here", multiple=false}).
-record(wizard, {?ELEMENT_BASE(element_wizard), tag, titles, steps, next="Next", back="Back", finish="Finish",show_progress=true,progress_step="Step ",progress_of=" of "}).
-record(sparkline, {?ELEMENT_BASE(element_sparkline), type, values, options }).
-record(textbox_autocomplete, {?ELEMENT_BASE(element_textbox_autocomplete), tag, text="", minLength=2, delay=300, html_encode=true, next, postback, delegate=undefined }).
-record(recaptcha, {?ELEMENT_BASE(element_recaptcha), captcha_opts=[], button_id, button_label="Check!", delegate, fail_body="Please try again!"}).
-record(textboxlist, {?ELEMENT_BASE(element_textboxlist), delegate, postback, unique=true, values=[], autocomplete=true, queryRemote=true, onlyFromValues=true, minLenght=1}).

        
%% HTML5 semantic elements
-record(section, {?ELEMENT_BASE(element_section), body=""}).
-record(nav, {?ELEMENT_BASE(element_nav), body=""}).
-record(article, {?ELEMENT_BASE(element_article), body=""}).
-record(aside, {?ELEMENT_BASE(element_aside), body=""}).
-record(hgroup, {?ELEMENT_BASE(element_hgroup), body=""}).
-record(html5_header, {?ELEMENT_BASE(element_html5_header), body=""}).
-record(html5_footer, {?ELEMENT_BASE(element_html5_footer), body=""}).
-record(time, {?ELEMENT_BASE(element_time), pubdate=false, datetime="", body=""}).
-record(mark, {?ELEMENT_BASE(element_mark), body=""}).

%%% Actions %%%
-define(AV_BASE(Module,Type), is_action=Type, module=Module, anchor, trigger, target, actions, show_if=true, source=[]).

-define(ACTION_BASE(Module), ?AV_BASE(Module,is_action)).

-record(actionbase, {?ACTION_BASE(undefined)}).
-record(wire, {?ACTION_BASE(action_wire)}).
-record(update, {?ACTION_BASE(action_update), type=update, elements=[]}).
-record(comet, {?ACTION_BASE(action_comet), pool=undefined, scope=local, function, dying_message}).
-record(continue, {?ACTION_BASE(action_continue), function, delegate, tag, timeout}).
-record(api, {?ACTION_BASE(action_api), name, tag, delegate }).
-record(function, {?ACTION_BASE(action_function), function }).
-record(set, {?ACTION_BASE(action_set), value}).
-record(redirect, {?ACTION_BASE(action_redirect), url, nodrop=false}).
-record(event, {?ACTION_BASE(action_event), type=default, keycode=undefined, shift_key=false, delay=0, postback, validation_group, delegate, extra_param}).
-record(control, {?ACTION_BASE(action_control), type=default, keycode=undefined, shift_key=false, delay=0, validation_group, delegate, extra_param}).
%% we want validation assignments to happen last, so we use AV_BASE and set deferral to zero first
-record(validate, {?ACTION_BASE(action_validate), on=submit, success_text=" ", group, validators, attach_to }).
-record(validation_error, {?ACTION_BASE(action_validation_error), text="" }).
-record(clear_validation, {?ACTION_BASE(action_clear_validation), validation_trigger, validation_target, validation_all}).
-record(alert, {?ACTION_BASE(action_alert), text=""}).
-record(confirm, {?ACTION_BASE(action_confirm), text="", postback, delegate}).
-record(script, {?ACTION_BASE(action_script), script}).
-record(disable_selection, {?ACTION_BASE(action_disable_selection)}).
-record(jquery_effect, {?ACTION_BASE(action_jquery_effect), type, effect, speed, options=[], class, easing}).
-record(show, {?ACTION_BASE(action_show), effect=none, options=[], speed=500}).
-record(hide, {?ACTION_BASE(action_hide), effect=none, options=[], speed=500}).
-record(appear, {?ACTION_BASE(action_appear), speed=500}).
-record(fade, {?ACTION_BASE(action_fade), speed=500}).
-record(slide_down, {?ACTION_BASE(action_slide_down), speed=500}).
-record(slide_up, {?ACTION_BASE(action_slide_up), speed=500}).
-record(effect, {?ACTION_BASE(action_effect), effect=none, options=[], speed=500}).
-record(toggle, {?ACTION_BASE(action_toggle), effect=none, options=[], speed=500}).
-record(add_class, {?ACTION_BASE(action_add_class), class=none, speed=0}).
-record(remove_class, {?ACTION_BASE(action_remove_class), class=none, speed=0}).
-record(animate, {?ACTION_BASE(action_animate), options=[], speed=500, easing=swing}).
-record(buttonize, {?ACTION_BASE(action_buttonize)}).
-record(disable, {?ACTION_BASE(action_disable)}).

%%% Validators %%%
%%% %% TODO: Switch this from is_action to is_validator once deferred is implemented
%%% This will allow users to bind validators directly, instead of needing the #validate{} action
-define(VALIDATOR_BASE(Module), ?AV_BASE(Module,is_action), text="Failed.").
-record(validatorbase, {?VALIDATOR_BASE(undefined)}).
-record(is_required, {?VALIDATOR_BASE(validator_is_required)}).
-record(is_email, {?VALIDATOR_BASE(validator_is_email)}).
-record(is_integer, {?VALIDATOR_BASE(validator_is_integer)}).
-record(min_length, {?VALIDATOR_BASE(validator_min_length), length}).
-record(max_length, {?VALIDATOR_BASE(validator_max_length), length}).
-record(confirm_password, {?VALIDATOR_BASE(validator_confirm_password), password}).
-record(custom, {?VALIDATOR_BASE(validator_custom), function, tag }).
-record(js_custom, {?VALIDATOR_BASE(validator_js_custom), function, args="{}" }).


-endif.
