// Micro BERT encoder/decoder
// Copyright (c) Maxim Sokhatsky (@5HT)

function atom(o) { return { type: "Atom", value: o, toString: function() { return this.value; } }; };
function bin(o) { return { type: "Binary", value: o, toString: function() { return "<<\'"+this.value+"'>>"; } }; };
function tuple() {
    return { type: "Tuple", value: arguments, toString: function() { var s = ""; 
        for (var i=0;i<this.value.length;i++) { if (s!=="") s+=","; s+=this.value[i]; }
        return "{" + s + "}"; } }; };
function dec(S) { return decode(ltoa(new Uint8Array(S))); };
function enc(s) {
    var ori = encode(s), buf = new Uint8Array(new ArrayBuffer(ori.length));
    for (var i=0; i < buf.length; i++) { buf[i] = ori.charCodeAt(i); }
    return new Blob([buf.buffer]); };

BERT = itoa(131);
SATOM = itoa(115);
ATOM = itoa(100);
BINARY = itoa(109);
SINT = itoa(97);
INT = itoa(98);
FLOAT = itoa(99);
STR = itoa(107);
LIST = itoa(108);
SBIG = itoa(110);
LBIG = itoa(111);
TUPLE = itoa(104);
LTUPLE = itoa(105);
NIL = itoa(106);
ZERO = itoa(0);

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
        Int = Math.floor(Int / 256);
    }
    if (Int > 0) { throw ("BERT: Range: " + OriginalInt); }
    return s; };
function ltoi(S, Length) {
    var isNegative, i, n, Num = 0;
    isNegative = (S.charCodeAt(0) > 128);
    for (i = 0; i < Length; i++) {
        n = S.charCodeAt(i);
        if (isNegative) { n = 255 - n; }
        if (Num === 0) { Num = n; }
        else { Num = Num * 256 + n; }
    }
    if (isNegative) { Num = Num * (0 - 1); }
    return Num; };
function ltobi(S, Count) {
    var isNegative, i, n, Num = 0;
    isNegative = (S.charCodeAt(0) === 1);
    S = S.substring(1);
    for (i = Count - 1; i >= 0; i--) {
        n = S.charCodeAt(i);
        if (Num === 0) { Num = n; } else { Num = Num * 256 + n; } }
    if (isNegative) { return Num * -1; }
    return Num; };

function encode(o) { return BERT + en_inner(o); };
function en_inner(Obj) { if(Obj === undefined) return NIL; var func = 'en_' + typeof(Obj); return eval(func)(Obj); };
function en_string(Obj) { return STR + itol(Obj.length, 2) + Obj; };
function en_boolean(Obj) { if (Obj) return en_inner(atom("true")); else return en_inner(atom("false")); };
function en_number(Obj) { var s, isi = (Obj % 1 === 0); if (!isi) { return en_float(Obj); }
    if (isi && Obj >= 0 && Obj < 256) { return SINT + itol(Obj, 1); }
    return INT + itol(Obj, 4); };
function en_float(Obj) { var s = Obj.toExponential(); while (s.length < 31) { s += ZERO; } return FLOAT + s; };
function en_object(Obj) {
    if (Obj.type === "Atom") return en_atom(Obj);
    if (Obj.type === "Binary") return en_bin(Obj);
    if (Obj.type === "Tuple") return en_tuple(Obj);
    if (Obj.constructor.toString().indexOf("Array") !== -1) return en_array(Obj);
    return en_associative_array(Obj); };
function en_atom(Obj) { return ATOM + itol(Obj.value.length, 2) + Obj.value; };
function en_bin(Obj) { return BINARY + itol(Obj.value.length, 4) + Obj.value; };
function en_tuple(Obj) {
    var i, s = "";
    if (Obj.value.length < 256) { s += TUPLE + itol(Obj.value.length, 1); }
    else { s += LTUPLE + itol(Obj.value.length, 4); }
    for (i = 0; i < Obj.value.length; i++) { s += en_inner(Obj.value[i]); }
    return s; };
function en_array(Obj) {
    var i, s = LIST + itol(Obj.length, 4);
    for (i = 0; i < Obj.length; i++) { s += en_inner(Obj[i]); }
    s += NIL;
    return s; };
function en_associative_array(Obj) {
    var key, Arr = [];
    for (key in Obj) { if (Obj.hasOwnProperty(key)) { Arr.push(tuple(atom(key), Obj[key])); } }
    return en_array(Arr); };

function decode(S) {
    if (S[0] !== BERT) { throw ("Not a valid BERT."); }
    var Obj = de_inner(S.substring(1));
    if (Obj.rest !== "") { throw ("Invalid BERT."); }
    return Obj.value; };
function de_inner(S) {
    var Type = S[0];
    S = S.substring(1);
    switch (Type) {
        case SATOM: de_atom(S, 1);
        case ATOM: return de_atom(S, 2);
        case BINARY: return de_bin(S);
        case SINT: return de_integer(S, 1);
        case INT: return de_integer(S, 4);
        case FLOAT: return de_float(S);
        case SBIG: return de_big(S,1);
        case LBIG: return de_big(S,4);
        case STR: return de_string(S);
        case LIST: return de_list(S);
        case TUPLE: return de_tuple(S, 1);
        case NIL: return de_nil(S);
        default: throw ("BERT: " + S.charCodeAt(0)); } };
function de_atom(S, Count) {
    var Size, Value;
    Size = ltoi(S, Count);
    S = S.substring(Count);
    Value = S.substring(0, Size);
    if (Value === "true") { Value = true; }
    else if (Value === "false") { Value = false; }
    return { value: atom(Value), rest:  S.substring(Size) }; };
function de_bin(S) {
    var Size = ltoi(S, 4);
    S = S.substring(4);
    return { value: bin(S.substring(0, Size)), rest: S.substring(Size) }; };
function de_big(S, Count) {
    var Size, Value;
    Size = ltoi(S, Count);
    S = S.substring(Count);
    Value = ltobi(S, Size);
    return { value : Value, rest: S.substring(Size + 1) }; };
function de_integer(S, Count) {
    var Value = ltoi(S, Count);
    S = S.substring(Count);
    return { value: Value, rest: S }; };
function de_float(S) {
    var Size = 31;
    return { value: parseFloat(S.substring(0, Size)), rest: S.substring(Size) }; };
function de_string(S) {
    var Size = ltoi(S, 2);
    S = S.substring(2);
    return { value: S.substring(0, Size), rest: S.substring(Size) }; };
function de_list(S) {
    var Size, i, El, LastChar, Arr = [];
    Size = ltoi(S, 4);
    S = S.substring(4);
    for (i = 0; i < Size; i++) { El = de_inner(S); Arr.push(El.value); S = El.rest; }
    LastChar = S[0];
    if (LastChar !== NIL) { throw ("BERT: Wrong NIL."); }
    S = S.substring(1);
    return { value: Arr, rest: S }; };
function de_tuple(S, Count) {
    var Size, i, El, Arr = [];
    Size = ltoi(S, Count);
    S = S.substring(Count);
    for (i = 0; i < Size; i++) { El = de_inner(S); Arr.push(El.value); S = El.rest; }
    return { value: tuple(Arr), rest: S }; };
function de_nil(S) { return { value: [], rest: S }; };
