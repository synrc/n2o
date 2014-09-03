
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
