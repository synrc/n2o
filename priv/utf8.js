try { module.exports = {dec:utf8_dec,enc:utf8_toByteArray}; } catch (e) { }

// N2O UTF-8 Support

function utf8_toByteArray(str) {
    var byteArray = [];
    if (str !== undefined && str !== null)
    for (var i = 0; i < str.length; i++)
        if (str.charCodeAt(i) <= 0x7F) byteArray.push(str.charCodeAt(i));
        else {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++) byteArray.push(parseInt(h[j], 16)); }
    return {t:107,v:byteArray}; };

function utf8_dec(ab) {
    if (!(ab instanceof ArrayBuffer)) ab = new Uint8Array(utf8_toByteArray(ab).v).buffer;
    var t=new DataView(ab),i=c=c1=c2=0,itoa=String.fromCharCode,s=[]; while (i<t.byteLength ) {
    c=t.getUint8(i); if (c<128) { s+=itoa(c); i++; } else
    if ((c>191) && (c<224)) { c2=t.getUint8(i+1); s+=itoa(((c&31)<<6)|(c2&63)); i+=2; }
    else { c2=t.getUint8(i+1); c3=t.getUint8(i+2); s+=itoa(((c&15)<<12)|((c2&63)<<6)|(c3&63));
    i+=3; } } return s; }
