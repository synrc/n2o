function atom(o) { return { type: "Atom", value: o, toString: function() { return this.value; } }; };
function bin(o) { return { type: "Binary", value: o, toString: function() { return this.value.length > 200 ? "<<["+this.value.length+"]>>" : "<<'"+utf8_decode(this.value)+"'>>"; } }; };
function tuple() { return { type: "Tuple", value: arguments, toString: function() { var s = "";
    for (var i=0;i<this.value.length;i++) { if (s!=="") s+=","; s+=this.value[i]; }
    return "{" + s + "}"; } }; };
function enc(s) { return new Blob([encodebuf(s).buffer]); };
function encodebuf(s) {
    var ori = encode(s), buf = new Uint8Array(new ArrayBuffer(ori.length));
    for (var i=0; i < buf.length; i++) { buf[i] = ori.charCodeAt(i); }
    return buf; }

function itoa(x) { return String.fromCharCode(x); }
function ltoa(a) { for (var i = 0,s=""; i < a.length; i++) s += itoa(a[i]); return s; };
function itol(Int, Length) {
    var isNegative, OriginalInt, i, Rem, s = "";
    isNegative = (Int < 0);
    if (isNegative) { Int = Int * (0 - 1); }
    OriginalInt = Int;
    for (i = 0; i < Length; i++) {
        Rem = Int % 256;
        if (isNegative) { Rem = 255 - Rem; }
        s = String.fromCharCode(Rem) + s;
        Int = Math.floor(Int / 256); }
    if (Int > 0) { throw ("BERT: Range: " + OriginalInt); }
    return s; };

function encode(o) { return itoa(131) + en_inner(o); };
function en_inner(Obj) { if(Obj === undefined) return 106; var func = 'en_' + typeof(Obj); return eval(func)(Obj); };
function en_string(Obj) { return itoa(107) + itol(Obj.length, 2) + Obj; };
function en_boolean(Obj) { if (Obj) return en_inner(atom("true")); else return en_inner(atom("false")); };
function en_number(Obj) { var isi = (Obj % 1 === 0); if (!isi) { return en_float(Obj); }
    if (isi && Obj >= 0 && Obj < 256) { return itoa(97) + itol(Obj, 1); }
    return itoa(98) + itol(Obj, 4); };
function en_float(Obj) { var s = Obj.toExponential(); while (s.length < 31) { s += 0; } return 99 + s; };
function en_object(Obj) {
    if (Obj.type === "Atom") return en_atom(Obj);
    if (Obj.type === "Binary") return en_bin(Obj);
    if (Obj.type === "Tuple") return en_tuple(Obj);
    if (Obj.constructor.toString().indexOf("Array") !== -1) return en_array(Obj); };
function en_atom(Obj) { return itoa(100) + itol(Obj.value.length, 2) + Obj.value; };
function en_bin(Obj) { return itoa(109) + itol(Obj.value.length, 4) + Obj.value; };
function en_tuple(Obj) {
    var i, s = "";
    if (Obj.value.length < 256) { s += itoa(104) + itol(Obj.value.length, 1); }
    else { s += itoa(105) + itol(Obj.value.length, 4); }
    for (i = 0; i < Obj.value.length; i++) { s += en_inner(Obj.value[i]); }
    return s; };
function en_array(Obj) {
    var i, s = itoa(108) + itol(Obj.length, 4);
    for (i = 0; i < Obj.length; i++) { s += en_inner(Obj[i]); }
    s += itoa(106);
    return s; };

dvp = DataView.prototype;
dvp.gu=dvp.getUint8;dvp.gu2=dvp.getUint16;
dvp.gi4=dvp.getInt32;dvp.gu4=dvp.getUint32;

var  $tab=
   [[ 97,int,1],[ 98,int,4],[ 99,nop,0],[100,str,2],
    [101,nop,0],[102,nop,0],[103,nop,0],[104,run,1],
    [105,run,4],[106,nop,4],[107,str,2],[108,run,4],
    [109,str,4],[110,nop,1],[111,nop,4],[112,nop,0],
    [113,nop,0],[114,nop,0],[115,nop,0],[116,nop,0],
    [117,nop,0],[118,nop,0],[119,nop,0],[  0,nop,0]],sx,ix;

function dec(d) { sx=new DataView(d);ix=0; if(sx.gu(ix++)!==131)throw("BERT?"); return din();};
function din()  { var c=sx.gu(ix++),x=(c>96&&c<120)?$tab[c-97]:$tab[23];
                  return {t:x[0],v:x[1](x[2])};};
function nop(b) { return []; };
function int(b) { return b==1?sx.gu(ix++):sx.gi4((a=ix,ix+=4,a)); };
function str(b) { var sz=b==2?sx.gu2(ix):sx.gi4(ix);ix+=b;
                  return utf8_dec(new DataView(sx.buffer.slice(ix,ix+=sz))); };
function run(b) { var sz=b==1?sx.gu(ix):sx.gu4(ix),r=[]; ix+=b;
                  for (var i=0;i<sz;i++)r.push(din());if(b==4)ix++; return r; };

function isTUPLE(x,num,name) { return (x.v.length === num && x.v[0].v === name); }
var $io = {}; $io.on = function onio(x, cb) { if (isTUPLE(x,3,'io')) {
    try { eval(x.v[1].v); if (typeof cb == 'function') cb(x); } catch (e) { console.log(e); return { status: '' }; }
    return { status: "ok" }; } else return { status: '' }; }
var $file = {}; $file.on = function onfile(x, cb) { if (isTUPLE(x,12,'ftp')) {
    if (typeof cb == 'function') cb(x); return { status: "ok" }; } else return { status: ''}; }
var $bin = {}; $bin.on = function onbin(x, cb) { if (isTUPLE(x,2,'bin')) {
    if (typeof cb == 'function') cb(x); return { status: "ok" }; } else return { status: '' }; }

var $bert = {}; $bert.protos = [$io,$bin,$file];
$bert.on = function onbert(evt, cb) {
    if (Blob.prototype.isPrototypeOf(evt.data) && (evt.data.length > 0 || evt.data.size > 0)) {
        var reader = new FileReader();
        reader.addEventListener("loadend", function() {
            try { lastBuf = reader.result;
                  erlang = dec(reader.result);
                  if (debug) console.log(JSON.stringify(erlang.v));
                  if (typeof cb  == 'function') cb(erlang);
                  for (var i=0;i<$bert.protos.length;i++) {
                    p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return; }
            } catch (e) { console.log(e); } });
        reader.readAsArrayBuffer(evt.data);
        return { status: "ok" }; } else return { status: "error", desc: "data" }; }
