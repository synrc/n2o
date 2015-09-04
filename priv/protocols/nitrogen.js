// Nitrogen Compatibility Layer

function querySourceRaw(Id) {
    var val, el = document.getElementById(Id);
    if (!el) return "";
    switch (el.tagName) {
        case 'FIELDSET': val = document.querySelector('#' + Id + ' :checked');
                         val = val ? val.value : ""; break;
        case 'INPUT':
            switch (el.getAttribute("type")) {
                case 'radio': case 'checkbox': val = el.checked ? el.value : ""; break;
                case  'date': val = getErlDate(el); break;
                case  'calendar': val = getDateFromCalendar(el); break;  //only 4 nitro #calendar{}
                default:     var edit = el.contentEditable;
                    if (edit && edit === 'true') val = el.innerHTML;
                    else val = el.value; }
            break;
        default: var edit = el.contentEditable;
            if (edit && edit === 'true') val = el.innerHTML;
            else val = el.value; }
    return val; }

function querySource(Id) {
//  if (Id.getValue) return bin(Id.getValue());
    var qs = querySourceRaw(Id);
    if ("" == qs) return bin('');
             else return utf8_toByteArray(qs); }

function getErlDate(el) {
    var val = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(el.value);
    if(val != null){
        var uDate = new Date(parseInt(val[1]),parseInt(val[2])-1,parseInt(val[3]));
        if( (uDate.getMonth()+1)==parseInt(val[2]) && (uDate.getDate())==parseInt(val[3]) && (uDate.getFullYear())==parseInt(val[1]) ) {
            val[2] = (uDate.getMonth() < 9) ? "0" + (uDate.getMonth()+1) : (uDate.getMonth()+1);
            val[3] = (uDate.getDate()  < 10) ? "0" + uDate.getDate() : uDate.getDate();
            val = "{"+ uDate.getFullYear() +","+ val[2] +","+ val[3] +"}";
        }else val = "";
    } else val = "";
    return val;
}

function getDateFromCalendar(el) {
    var picker = pickers[el.id] || null;
    var val = {};
    if(picker) {
        val.month = (picker._d.getMonth() < 9) ? "0" + (picker._d.getMonth()+1) : (picker._d.getMonth()+1);
        val.day = (picker._d.getDate()  < 10) ? "0" + picker._d.getDate() : picker._d.getDate();
        val.erlangDate = "{"+ picker._d.getFullYear() +","+ val.month +","+ val.day +"}";
    }else { val.erlangDate = ""; }
    return val.erlangDate;
}
