
// N2O UTF-8 Support

function utf8_dec(ab) { return (new TextDecoder()).decode(ab); }
function utf8_enc(ab) { return (new TextEncoder("utf-8")).encode(ab); }
function utf8_arr(ab) { if (!(ab instanceof ArrayBuffer)) ab = new Uint8Array(utf8_enc(ab)).buffer;
                        return utf8_dec(ab); }
