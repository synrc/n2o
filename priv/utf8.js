
// N2O UTF-8 Support

function utf8_toByteArray(str) {
    var byteArray = [];
    if (str !== undefined && str !== null)
    for (var i = 0; i < str.length; i++)
        if (str.charCodeAt(i) <= 0x7F) byteArray.push(str.charCodeAt(i));
        else {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++) byteArray.push(parseInt(h[j], 16)); }
    return byteArray; };

function utf8_fromByteArray(byteArray, separator) {
    if (typeof byteArray == 'undefined' || byteArray.byteLength == 0) { return "" };
    separator = typeof separator !== 'undefined' ? separator : ',';
    var dataView = new DataView(byteArray);
    var s = dataView.getUint8(0).toString();
    for (var i = 1; i < byteArray.byteLength; i++)
        s = s + separator + dataView.getUint8(i).toString();
    return s; }

function utf8_decode(utftext) {
    var string = "";
    var i = c = c1 = c2 = 0;
    while ( i < utftext.length ) {
        c = utftext.charCodeAt(i);
        if (c < 128) {
            string += String.fromCharCode(c);
            i++;
        } else if ((c > 191) && (c < 224)) {
            c2 = utftext.charCodeAt(i+1);
            string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
            i += 2;
        } else {
            c2 = utftext.charCodeAt(i+1);
            c3 = utftext.charCodeAt(i+2);
            string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
            i += 3;
        }
    }
    return string;
}
