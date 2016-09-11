// Nitrogen Compatibility Layer

function querySourceRaw(Id) {
    var val, el = document.getElementById(Id);
    if (!el) return "";
    switch (el.tagName) {
        case 'FIELDSET': val = document.querySelector('[id="' + Id + '"] :checked');
                         val = val ? val.value : ""; break;
        case 'INPUT':
            switch (el.getAttribute("type")) {
                case 'radio': case 'checkbox': val = el.checked ? el.value : ""; break;
                case  'date': val = new Date(Date.parse(el.value)) || ""; break;
                case  'calendar': val = pickers[el.id]._d || ""; break;  //only 4 nitro #calendar{}
                default:     var edit = el.contentEditable;
                    if (edit && edit === 'true') val = el.innerHTML;
                    else val = el.value; }
            break;
        default: var edit = el.contentEditable;
            if (edit && edit === 'true') val = el.innerHTML;
            else val = el.value; }
    return val; }

function querySource(Id) {
    var qs = querySourceRaw(Id);
    if(qs instanceof Date) { return tuple(number(qs.getFullYear()),number(qs.getMonth()+1),number(qs.getDate())); }
    else { return utf8_toByteArray(qs); } }

(function() {
    window.requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
        window.webkitRequestAnimationFrame || window.msRequestAnimationFrame; })();
