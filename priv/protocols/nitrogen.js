// Nitrogen Compatibility Layer

function querySourceRaw(Id) {
    var val, el = document.getElementById(Id);
    if (!el) return "";
    switch (el.type) {
        case 'fieldset': val = document.querySelector('#' + Id + ' :checked');
                         val = val ? val.value : ""; break;
        case 'radio': case 'checkbox': val = el.checked ? el.value : ""; break;
        default:         var edit = el.getAttribute('contenteditable');
                         if (edit && edit === 'true') val = el.innerHTML;
                                                 else val = el.value; }
    return val; }

function querySource(Id) {
//  if (Id.getValue) return bin(Id.getValue());
    var qs = querySourceRaw(Id);
    if ("" == qs) return bin('');
             else return utf8_toByteArray(qs); }
