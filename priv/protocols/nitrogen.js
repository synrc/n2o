
// Nitrogen Compatibility Layer

function querySource(Id)
{
    if (Id.getValue) return bin(Id.getValue());
    var val, el = document.getElementById(Id);
    if (!el) return atom('undefined');

    switch (el.type)
    {
        case 'fieldset': val = document.querySelector('#' + Id + ' :checked');
                         val = val ? utf8.toByteArray(val.value) : utf8.toByteArray("");
                         break;
        case 'radio':
        case 'checkbox': val = el.checked ? el.value : atom('undefined');
                         break;
        default:         var edit = el.getAttribute('contenteditable');
                         if (edit && edit === 'true') val = bin(el.innerHTML);
                                                 else val = utf8_toByteArray(el.value);
    }

    return val;
}
