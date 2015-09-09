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
    if (Obj.constructor.toString().indexOf("Array") !== -1) return en_array(Obj);
    return en_associative_array(Obj); };
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
function en_associative_array(Obj) {
    var key, Arr = [];
    for (key in Obj) { if (Obj.hasOwnProperty(key)) { Arr.push(tuple(atom(key), Obj[key])); } }
    return en_array(Arr); };

dvp = DataView.prototype;
dvp.gu=dvp.getUint8;dvp.gu2=dvp.getUint16;
dvp.gi4=dvp.getInt32;dvp.gu4=dvp.getUint32;

function nop(b) { return []; };
function int(b) { return b==1?sx.gu(ix++):sx.gi4((a=ix,ix+=4,a)); };
function dec(d) { sx=new DataView(d);ix=0; if(sx.gu(ix++)!==131)throw("BERT?"); return din();};
function str(b) { var sz=b==2?sx.gu2(ix):sx.gi4(ix);ix+=b;
                  return utf8_dec(new DataView(sx.buffer.slice(ix,ix+=sz))); };
function run(b) { var i=sz=b==1?sx.gu(ix):sx.gu4(ix),r=[]; ix+=b;
                  while(i--) r[sz-i-1]=din(); if(b==4)ix++; return r; };
function din()  { var c=sx.gu(ix++),x; switch(c) { case 97: x=[int,1];break;
                  case 98:  x=[int,4]; break; case 100: x=[str,2]; break;
                  case 104: x=[run,1]; break; case 107: x=[str,2]; break;
                  case 108: x=[run,4]; break; case 109: x=[str,4]; break;
                  default:  x=[nop,0]; } return {t:c,v:x[0](x[1])};};

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
        var r = new FileReader();
        r.addEventListener("loadend", function() {
            try { erlang = dec(r.result);
                  if (debug) console.log(JSON.stringify(erlang.v));
                  if (typeof cb  == 'function') cb(erlang);
                  for (var i=0;i<$bert.protos.length;i++) {
                    p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return; }
            } catch (e) { console.log(e); } });
        r.readAsArrayBuffer(evt.data);
        return { status: "ok" }; } else return { status: "error", desc: "data" }; }
