try { module.exports = { dec: utf8_dec, enc: utf8_toByteArray }; } catch (e) { }

// N2O UTF-8 Support

function utf8_toByteArray(str) { return { t: 107, v: (new TextEncoder("utf-8")).encode(str) }; };
function utf8_dec(ab) { if (!(ab instanceof ArrayBuffer)) ab = new Uint8Array(utf8_toByteArray(ab).v).buffer;
                        return (new TextDecoder()).decode(ab); }
