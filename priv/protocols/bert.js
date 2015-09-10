try { module.exports = {dec:dec,enc:enc}; } catch (e) { }

// BERT Encoder

function ar(o) { return Array.isArray(o.v)?o.v:utf8_toByteArray(o.v).v;}
function fl(a) { return a.reduce(function(f,t){return f.concat(Array.isArray(t)?fl(t):t);},[]);}
function atom(o)   { return {t:100,v:utf8_toByteArray(o).v}; }
function bin(o)    { return {t:109,v:utf8_toByteArray(o).v}; }
function tuple()   { return {t:104,v:Array.apply(null,arguments)}; }
function list()    { return {t:108,v:Array.apply(null,arguments)}; }
function number(o) { return {t:98,v:o}; }
function enc(o)    { return new Uint8Array(fl([131,ein(o)])); }
function ein(o)    { return Array.isArray(o)?en_108({t:108,v:o}):eval('en_'+o.t)(o); }
function en_undefined(o) { return [106]; }
function en_98(o)  { return [98,o.v>>>24,(o.v>>>16)&255,(o.v>>>8)&255,o.v&255]; }
function en_97(o)  { return [97,o.v];}
function en_106(o) { return [106];}
function en_100(o) { return [100,o.v.length>>>8,o.v.length&255,ar(o)]; }
function en_107(o) { return [107,o.v.length>>>8,o.v.length&255,ar(o)];}
function en_104(o) { var l=o.v.length,r=[]; for(var i=0;i<l;i++)r[i]=ein(o.v[i]); return [104,l,r]; }
function en_109(o) { var l=o.v.length;return[109,l>>>24,(l>>>16)&255,(l>>>8)&255,l&255,ar(o)]; }
function en_108(o) { var l=o.v.length,r=[]; for(var i=0;i<l;i++)r.push(ein(o.v[i]));
                     return o.v.length==0?[106]:[108,l>>>24,(l>>>16)&255,(l>>>8)&255,l&255,r,106]; }

// BERT Decoder

function nop(b) { return []; };
function int(b) { return b==1?sx.getUint8(ix++):sx.getInt32((a=ix,ix+=4,a)); };
function dec(d) { sx=new DataView(d);ix=0; if(sx.getUint8(ix++)!==131)throw("BERT?"); return din();};
function str(b) { var dv,sz=(b==2?sx.getUint16(ix):sx.getInt32(ix));ix+=b;
                  return utf8_dec(new DataView(sx.buffer.slice(ix,ix+=sz))); };
function run(b) { var sz=(b==1?sx.getUint8(ix):sx.getUint32(ix)),r=[]; ix+=b;
                  for(var i=0;i<sz;i++) r.push(din()); if(b==4)ix++; return r; };
function din()  { var c=sx.getUint8(ix++),x; switch(c) { case 97: x=[int,1];break;
                  case 98:  x=[int,4]; break; case 100: x=[str,2]; break;
                  case 104: x=[run,1]; break; case 107: x=[str,2]; break;
                  case 108: x=[run,4]; break; case 109: x=[str,4]; break;
                  default:  x=[nop,0]; } return {t:c,v:x[0](x[1])};};
