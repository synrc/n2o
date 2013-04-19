// BERT-JS
// Copyright (c) 2009 Rusty Klophaus (@rklophaus)
// Contributions by Ben Browning (@bbrowning)
// See MIT-LICENSE for licensing information.


// BERT-JS is a Javascript implementation of Binary Erlang Term Serialization.
// - http://github.com/rklophaus/BERT-JS
//
// References:
// - http://www.erlang-factory.com/upload/presentations/36/tom_preston_werner_erlectricity.pdf
// - http://www.erlang.org/doc/apps/erts/erl_ext_dist.html#8


// - CLASSES -

function BertClass() {
	this.BERT_START = String.fromCharCode(131);
	this.SMALL_ATOM = String.fromCharCode(115);
	this.ATOM = String.fromCharCode(100);
	this.BINARY = String.fromCharCode(109);
	this.SMALL_INTEGER = String.fromCharCode(97);
	this.INTEGER = String.fromCharCode(98);
	this.SMALL_BIG = String.fromCharCode(110);
	this.LARGE_BIG = String.fromCharCode(111);
	this.FLOAT = String.fromCharCode(99);
	this.STRING = String.fromCharCode(107);
	this.LIST = String.fromCharCode(108);
	this.SMALL_TUPLE = String.fromCharCode(104);
	this.LARGE_TUPLE = String.fromCharCode(105);
	this.NIL = String.fromCharCode(106);
	this.ZERO = String.fromCharCode(0);
}

function BertAtom(Obj) {
	this.type = "Atom";
	this.value = Obj;
	this.toString = function () {
		return Obj;
	};
}

function BertBinary(Obj) {
	this.type = "Binary";
	this.value = Obj;
	this.toString = function () {
		return "<<\"" + Obj + "\">>";
	};
}

function BertTuple(Arr) {
	this.type = "Tuple";
	this.length = Arr.length;
	this.value = Arr;
	for (var i = 0; i < Arr.length; i++) {
		this[i] = Arr[i];
	}
	this.toString = function () {
		var i, s = "";
		for (i = 0; i < this.length; i++) {
			if (s !== "") {
				s += ", ";
			}
			s += this[i].toString();
		}

		return "{" + s + "}";
	};
}



// - INTERFACE -

BertClass.prototype.encode = function (Obj) {
	return this.BERT_START + this.encode_inner(Obj);
};

BertClass.prototype.decode = function (S) {
	if (S[0] !== this.BERT_START) {
		throw ("Not a valid BERT.");
	}
	var Obj = this.decode_inner(S.substring(1));
	if (Obj.rest !== "") {
		throw ("Invalid BERT.");
	}
	return Obj.value;
};

BertClass.prototype.atom = function (Obj) {
	return new BertAtom(Obj);
};

BertClass.prototype.binary = function (Obj) {
	return new BertBinary(Obj);
};

BertClass.prototype.tuple = function () {
	return new BertTuple(arguments);
};



// - ENCODING -

BertClass.prototype.encode_inner = function (Obj) {
	var func = 'encode_' + typeof(Obj);
	return this[func](Obj);
};

BertClass.prototype.encode_string = function (Obj) {
	return this.STRING + this.int_to_bytes(Obj.length, 2) + Obj;
};

BertClass.prototype.encode_boolean = function (Obj) {
	if (Obj) {
		return this.encode_inner(this.atom("true"));
	}
	else {
		return this.encode_inner(this.atom("false"));
	}
};

BertClass.prototype.encode_number = function (Obj) {
	var s, isInteger = (Obj % 1 === 0);

	// Handle floats...
	if (!isInteger) {
		return this.encode_float(Obj);
	}

	// Small int...
	if (isInteger && Obj >= 0 && Obj < 256) {
		return this.SMALL_INTEGER + this.int_to_bytes(Obj, 1);
	}

	// 4 byte int...
	if (isInteger && Obj >= -134217728 && Obj <= 134217727) {
		return this.INTEGER + this.int_to_bytes(Obj, 4);
	}

	// Bignum...
	s = this.bignum_to_bytes(Obj);
	if (s.length < 256) {
		return this.SMALL_BIG + this.int_to_bytes(s.length - 1, 1) + s;
	} else {
		return this.LARGE_BIG + this.int_to_bytes(s.length - 1, 4) + s;
	}
};

BertClass.prototype.encode_float = function (Obj) {
	// float...
	var s = Obj.toExponential();
	while (s.length < 31) {
		s += this.ZERO;
	}
	return this.FLOAT + s;
};

BertClass.prototype.encode_object = function (Obj) {
	// Check if it's an atom, binary, or tuple...
	if (Obj.type === "Atom") {
		return this.encode_atom(Obj);
	}
	if (Obj.type === "Binary") {
		return this.encode_binary(Obj);
	}
	if (Obj.type === "Tuple") {
		return this.encode_tuple(Obj);
	}

	// Check if it's an array...
	if (Obj.constructor.toString().indexOf("Array") !== -1) {
		return this.encode_array(Obj);
	}

	// Treat the object as an associative array...
	return this.encode_associative_array(Obj);
};

BertClass.prototype.encode_atom = function (Obj) {
	return this.ATOM + this.int_to_bytes(Obj.value.length, 2) + Obj.value;
};

BertClass.prototype.encode_binary = function (Obj) {
	return this.BINARY + this.int_to_bytes(Obj.value.length, 4) + Obj.value;
};

BertClass.prototype.encode_tuple = function (Obj) {
	var i, s = "";
	if (Obj.length < 256) {
		s += this.SMALL_TUPLE + this.int_to_bytes(Obj.length, 1);
	} else {
		s += this.LARGE_TUPLE + this.int_to_bytes(Obj.length, 4);
	}
	for (i = 0; i < Obj.length; i++) {
		s += this.encode_inner(Obj[i]);
	}
	return s;
};

BertClass.prototype.encode_array = function (Obj) {
	var i, s = this.LIST + this.int_to_bytes(Obj.length, 4);
	for (i = 0; i < Obj.length; i++) {
		s += this.encode_inner(Obj[i]);
	}
	s += this.NIL;
	return s;
};

BertClass.prototype.encode_associative_array = function (Obj) {
	var key, Arr = [];
	for (key in Obj) {
		if (Obj.hasOwnProperty(key)) {
			Arr.push(this.tuple(this.atom(key), Obj[key]));
		}
	}
	return this.encode_array(Arr);
};



// - DECODING -

BertClass.prototype.decode_inner = function (S) {
	var Type = S[0];
	S = S.substring(1);
	switch (Type) {
	case this.SMALL_ATOM:
		return this.decode_atom(S, 1);
	case this.ATOM:
		return this.decode_atom(S, 2);
	case this.BINARY:
		return this.decode_binary(S);
	case this.SMALL_INTEGER:
		return this.decode_integer(S, 1);
	case this.INTEGER:
		return this.decode_integer(S, 4);
	case this.SMALL_BIG:
		return this.decode_big(S, 1);
	case this.LARGE_BIG:
		return this.decode_big(S, 4);
	case this.FLOAT:
		return this.decode_float(S);
	case this.STRING:
		return this.decode_string(S);
	case this.LIST:
		return this.decode_list(S);
	case this.SMALL_TUPLE:
		return this.decode_tuple(S, 1);
	case this.LARGE_TUPLE:
		return this.decode_large_tuple(S, 4);
	case this.NIL:
		return this.decode_nil(S);
	default:
		throw ("Unexpected BERT type: " + S.charCodeAt(0));
	}
};

BertClass.prototype.decode_atom = function (S, Count) {
	var Size, Value;
	Size = this.bytes_to_int(S, Count);
	S = S.substring(Count);
	Value = S.substring(0, Size);
	if (Value === "true") {
		Value = true;
	}
	else if (Value === "false") {
		Value = false;
	}
	return {
		value: this.atom(Value),
		rest:  S.substring(Size)
	};
};

BertClass.prototype.decode_binary = function (S) {
	var Size = this.bytes_to_int(S, 4);
	S = S.substring(4);
	return {
		value: this.binary(S.substring(0, Size)),
		rest:  S.substring(Size)
	};
};

BertClass.prototype.decode_integer = function (S, Count) {
	var Value = this.bytes_to_int(S, Count);
	S = S.substring(Count);
	return {
		value: Value,
		rest:  S
	};
};

BertClass.prototype.decode_big = function (S, Count) {
	var Size, Value;
	Size = this.bytes_to_int(S, Count);
	S = S.substring(Count);
	Value = this.bytes_to_bignum(S, Size);
	return {
		value : Value,
		rest: S.substring(Size + 1)
	};
};

BertClass.prototype.decode_float = function (S) {
	var Size = 31;
	return {
		value: parseFloat(S.substring(0, Size)),
		rest: S.substring(Size)
	};
};

BertClass.prototype.decode_string = function (S) {
	var Size = this.bytes_to_int(S, 2);
	S = S.substring(2);
	return {
		value: S.substring(0, Size),
		rest:  S.substring(Size)
	};
};

BertClass.prototype.decode_list = function (S) {
	var Size, i, El, LastChar, Arr = [];
	Size = this.bytes_to_int(S, 4);
	S = S.substring(4);
	for (i = 0; i < Size; i++) {
		El = this.decode_inner(S);
		Arr.push(El.value);
		S = El.rest;
	}
	LastChar = S[0];
	if (LastChar !== this.NIL) {
		throw ("List does not end with NIL!");
	}
	S = S.substring(1);
	return {
		value: Arr,
		rest: S
	};
};

BertClass.prototype.decode_tuple = function (S, Count) {
	var Size, i, El, Arr = [];
	Size = this.bytes_to_int(S, Count);
	S = S.substring(Count);
	for (i = 0; i < Size; i++) {
		El = this.decode_inner(S);
		Arr.push(El.value);
		S = El.rest;
	}
	return {
		value: this.tuple(Arr),
		rest: S
	};
};

BertClass.prototype.decode_nil = function (S) {
	// nil is an empty list
	return {
		value: [],
		rest: S
	};
};



// - UTILITY FUNCTIONS -

// Encode an integer to a big-endian byte-string of length Length.
// Throw an exception if the integer is too large
// to fit into the specified number of bytes.
BertClass.prototype.int_to_bytes = function (Int, Length) {
	var isNegative, OriginalInt, i, Rem, s = "";
	isNegative = (Int < 0);
	if (isNegative) {
		Int = Int * (0 - 1);
	}
	OriginalInt = Int;
	for (i = 0; i < Length; i++) {
		Rem = Int % 256;
		if (isNegative) {
			Rem = 255 - Rem;
		}
		s = String.fromCharCode(Rem) + s;
		Int = Math.floor(Int / 256);
	}
	if (Int > 0) {
		throw ("Argument out of range: " + OriginalInt);
	}
	return s;
};

// Read a big-endian encoded integer from the first Length bytes
// of the supplied string.
BertClass.prototype.bytes_to_int = function (S, Length) {
	var isNegative, i, n, Num = 0;
	isNegative = (S.charCodeAt(0) > 128);
	for (i = 0; i < Length; i++) {
		n = S.charCodeAt(i);
		if (isNegative) {
			n = 255 - n;
		}
		if (Num === 0) {
			Num = n;
		}
		else {
			Num = Num * 256 + n;
		}
	}
	if (isNegative) {
		Num = Num * (0 - 1);
	}
	return Num;
};

// Encode an integer into an Erlang bignum,
// which is a byte of 1 or 0 representing
// whether the number is negative or positive,
// followed by little-endian bytes.
BertClass.prototype.bignum_to_bytes = function (Int) {
	var isNegative, Rem, s = "";
	isNegative = Int < 0;
	if (isNegative) {
		Int *= -1;
		s += String.fromCharCode(1);
	} else {
		s += String.fromCharCode(0);
	}

	while (Int !== 0) {
		Rem = Int % 256;
		s += String.fromCharCode(Rem);
		Int = Math.floor(Int / 256);
	}

	return s;
};

// Encode a list of bytes into an Erlang bignum.
BertClass.prototype.bytes_to_bignum = function (S, Count) {
	var isNegative, i, n, Num = 0;
	isNegative = (S.charCodeAt(0) === 1);
	S = S.substring(1);
	for (i = Count - 1; i >= 0; i--) {
		n = S.charCodeAt(i);
		if (Num === 0) {
			Num = n;
		}
		else {
			Num = Num * 256 + n;
		}
	}
	if (isNegative) {
		return Num * -1;
	}
	return Num;
};

// Convert an array of bytes into a string.
BertClass.prototype.bytes_to_string = function (Arr) {
	var i, s = "";
	for (i = 0; i < Arr.length; i++) {
		s += String.fromCharCode(Arr[i]);
	}
	return s;
};

// - TESTING -

// Pretty Print a byte-string in Erlang binary form.
BertClass.prototype.pp_bytes = function (Bin) {
	var i, s = "";
	for (i = 0; i < Bin.length; i++) {
		if (s !== "") {
			s += ",";
		}
		s += "" + Bin.charCodeAt(i);
	}
	return "<<" + s + ">>";
};

// Pretty Print a JS object in Erlang term form.
BertClass.prototype.pp_term = function (Obj) {
	return Obj.toString();
};

// Show off the different type of encodings we
// can handle.
BertClass.prototype.test_encode = function () {
	alert(this.pp_bytes(this.encode(this.atom("hello"))));
	alert(this.pp_bytes(this.encode(this.binary("hello"))));
	alert(this.pp_bytes(this.encode(true)));
	alert(this.pp_bytes(this.encode(42)));
	alert(this.pp_bytes(this.encode(5000)));
	alert(this.pp_bytes(this.encode(-5000)));
	alert(this.pp_bytes(this.encode(987654321)));
	alert(this.pp_bytes(this.encode(-987654321)));
	alert(this.pp_bytes(this.encode(3.14159)));
	alert(this.pp_bytes(this.encode(-3.14159)));
	alert(this.pp_bytes(this.encode([1, 2, 3])));
	alert(this.pp_bytes(this.encode({a : 1, b : 2, c : 3})));
	alert(this.pp_bytes(this.encode(this.tuple("Hello", 1))));
	alert(this.pp_bytes(this.encode([])));
	alert(this.pp_bytes(this.encode({
		a : this.tuple(1, 2, 3),
		b : [4, 5, 6]
	})));

};

BertClass.prototype.test_decode = function () {
	var TestTerm1, TestTerm2, TestTerm3, TestTerm4;
	// Try decoding this: [{atom, myAtom},{binary, <<"My Binary">>},{bool, true}, {string, "Hello there"}],
	TestTerm1 = this.bytes_to_string([131, 108, 0, 0, 0, 4, 104, 2, 100, 0, 4, 97, 116, 111, 109, 100, 0, 6, 109, 121, 65, 116, 111, 109, 104, 2, 100, 0, 6, 98, 105, 110, 97, 114, 121, 109, 0, 0, 0, 9, 77, 121, 32, 66, 105, 110, 97, 114, 121, 104, 2, 100, 0, 4, 98, 111, 111, 108, 100, 0, 4, 116, 114, 117, 101, 104, 2, 100, 0, 6, 115, 116, 114, 105, 110, 103, 107, 0, 11, 72, 101, 108, 108, 111, 32, 116, 104, 101, 114, 101, 106]);
	alert(this.pp_term(this.decode(TestTerm1)));

	// Try decoding this: [{small_integer, 42},{integer1, 5000},{integer2, -5000},{big_int1, 987654321},{big_int2, -987654321}],
	TestTerm2 = this.bytes_to_string([131, 108, 0, 0, 0, 5, 104, 2, 100, 0, 13, 115, 109, 97, 108, 108, 95, 105, 110, 116, 101, 103, 101, 114, 97, 42, 104, 2, 100, 0, 8, 105, 110, 116, 101, 103, 101, 114, 49, 98, 0, 0, 19, 136, 104, 2, 100, 0, 8, 105, 110, 116, 101, 103, 101, 114, 50, 98, 255, 255, 236, 120, 104, 2, 100, 0, 8, 98, 105, 103, 95, 105, 110, 116, 49, 110, 4, 0, 177, 104, 222, 58, 104, 2, 100, 0, 8, 98, 105, 103, 95, 105, 110, 116, 50, 110, 4, 1, 177, 104, 222, 58, 106]);
	alert(this.pp_term(this.decode(TestTerm2)));

	// Try decoding this: -3.14159
	TestTerm3 = this.bytes_to_string([131, 99, 45, 51, 46, 49, 52, 49, 53, 56, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 56, 56, 50, 54, 50, 101, 43, 48, 48, 0, 0, 0, 0]);
	alert(this.pp_term(this.decode(TestTerm3)));

	// Try decoding this: [] (empty list)
	TestTerm4 = this.bytes_to_string([131, 106]);
	alert(this.pp_term(this.decode(TestTerm4)));
};

var Bert = new BertClass();

// Bert.test_encode();
// Bert.test_decode();
