(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.cl.bh === region.cP.bh)
	{
		return 'on line ' + region.cl.bh;
	}
	return 'on lines ' + region.cl.bh + ' through ' + region.cP.bh;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eA,
		impl.dz,
		impl.ds,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		Z: func(record.Z),
		cm: record.cm,
		ch: record.ch
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.Z;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.cm;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ch) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eA,
		impl.dz,
		impl.ds,
		function(sendToApp, initialModel) {
			var view = impl.fv;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eA,
		impl.dz,
		impl.ds,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bl && impl.bl(sendToApp)
			var view = impl.fv;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.dY);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bp) && (_VirtualDom_doc.title = title = doc.bp);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.eN;
	var onUrlRequest = impl.eO;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bl: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.di === next.di
							&& curr.cW === next.cW
							&& curr.df.a === next.df.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		eA: function(flags)
		{
			return A3(impl.eA, flags, _Browser_getUrl(), key);
		},
		fv: impl.fv,
		dz: impl.dz,
		ds: impl.ds
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { et: 'hidden', d5: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { et: 'mozHidden', d5: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { et: 'msHidden', d5: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { et: 'webkitHidden', d5: 'webkitvisibilitychange' }
		: { et: 'hidden', d5: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		$7: _Browser_getScene(),
		dC: {
			ai: _Browser_window.pageXOffset,
			aj: _Browser_window.pageYOffset,
			cp: _Browser_doc.documentElement.clientWidth,
			b8: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cp: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		b8: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			$7: {
				cp: node.scrollWidth,
				b8: node.scrollHeight
			},
			dC: {
				ai: node.scrollLeft,
				aj: node.scrollTop,
				cp: node.clientWidth,
				b8: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			$7: _Browser_getScene(),
			dC: {
				ai: x,
				aj: y,
				cp: _Browser_doc.documentElement.clientWidth,
				b8: _Browser_doc.documentElement.clientHeight
			},
			el: {
				ai: x + rect.left,
				aj: y + rect.top,
				cp: rect.width,
				b8: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2(elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}var author$project$Main$LinkClicked = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$UrlChanged = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$NotFound = {$: 0};
var author$project$Base$base = 'dnd-list';
var author$project$Configuration$Groups$InsertAfter$Item = F3(
	function (group, value, color) {
		return {b3: color, t: group, co: value};
	});
var author$project$Configuration$Groups$InsertAfter$blue = '#45858c';
var author$project$Configuration$Groups$InsertAfter$green = '#858c45';
var author$project$Configuration$Groups$InsertAfter$red = '#8c4585';
var author$project$Configuration$Groups$InsertAfter$transparent = 'transparent';
var elm$core$Basics$EQ = 1;
var elm$core$Basics$LT = 0;
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = 2;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var author$project$Configuration$Groups$InsertAfter$gatheredByGroup = _List_fromArray(
	[
		A3(author$project$Configuration$Groups$InsertAfter$Item, 0, '', author$project$Configuration$Groups$InsertAfter$transparent),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 0, '2', author$project$Configuration$Groups$InsertAfter$red),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 0, 'B', author$project$Configuration$Groups$InsertAfter$blue),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 0, 'A', author$project$Configuration$Groups$InsertAfter$blue),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 0, 'III', author$project$Configuration$Groups$InsertAfter$green),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 1, '', author$project$Configuration$Groups$InsertAfter$transparent),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 1, 'C', author$project$Configuration$Groups$InsertAfter$blue),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 1, '1', author$project$Configuration$Groups$InsertAfter$red),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 1, 'I', author$project$Configuration$Groups$InsertAfter$green),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 1, '3', author$project$Configuration$Groups$InsertAfter$red),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 2, '', author$project$Configuration$Groups$InsertAfter$transparent),
		A3(author$project$Configuration$Groups$InsertAfter$Item, 2, 'II', author$project$Configuration$Groups$InsertAfter$green)
	]);
var author$project$Configuration$Groups$InsertAfter$MyMsg = elm$core$Basics$identity;
var elm$core$Basics$eq = _Utils_equal;
var author$project$Configuration$Groups$InsertAfter$compareByGroup = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.t, dropItem.t);
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var author$project$Configuration$Groups$InsertAfter$updateOnGroupChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{t: dropItem.t});
								}),
							_List_fromArray(
								[item]),
							drop) : _List_fromArray(
							[item]);
					}),
				list));
	});
var author$project$DnDList$Groups$Free = 0;
var author$project$DnDList$Groups$InsertAfter = 0;
var author$project$DnDList$Groups$OnDrag = 0;
var author$project$DnDList$Groups$Swap = 4;
var author$project$Configuration$Groups$InsertAfter$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Configuration$Groups$InsertAfter$updateOnGroupChange, eb: author$project$Configuration$Groups$InsertAfter$compareByGroup, c8: 0},
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$DnDList$Groups$Draggable = elm$core$Basics$identity;
var author$project$DnDList$Groups$GotSourceElement = function (a) {
	return {$: 6, a: a};
};
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$Maybe$Nothing = {$: 1};
var elm$core$Basics$False = 1;
var elm$core$Basics$True = 0;
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.g) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.j),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.j);
		} else {
			var treeLen = builder.g * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.k) : builder.k;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.g);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.j) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.j);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{k: nodeList, g: (len / elm$core$Array$branchFactor) | 0, j: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {cS: fragment, cW: host, dd: path, df: port_, di: protocol, dj: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Dom$getElement = _Browser_getElement;
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			A2(
				elm$core$Task$onError,
				A2(
					elm$core$Basics$composeL,
					A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
					elm$core$Result$Err),
				A2(
					elm$core$Task$andThen,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Ok),
					task)));
	});
var author$project$DnDList$Groups$sourceCommands = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Cmd$none;
		} else {
			var m = model.a;
			var _n2 = m.af;
			if (_n2.$ === 1) {
				return A2(
					elm$core$Task$attempt,
					A2(elm$core$Basics$composeL, wrap, author$project$DnDList$Groups$GotSourceElement),
					elm$browser$Browser$Dom$getElement(m.a5));
			} else {
				return elm$core$Platform$Cmd$none;
			}
		}
	});
var author$project$DnDList$Groups$GotTargetElement = function (a) {
	return {$: 7, a: a};
};
var author$project$DnDList$Groups$targetCommands = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Cmd$none;
		} else {
			var m = model.a;
			return (!m.m) ? A2(
				elm$core$Task$attempt,
				A2(elm$core$Basics$composeL, wrap, author$project$DnDList$Groups$GotTargetElement),
				elm$browser$Browser$Dom$getElement(m.az)) : elm$core$Platform$Cmd$none;
		}
	});
var author$project$DnDList$Groups$commands = F2(
	function (wrap, draggable) {
		return elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2(author$project$DnDList$Groups$sourceCommands, wrap, draggable),
					A2(author$project$DnDList$Groups$targetCommands, wrap, draggable)
				]));
	});
var author$project$DnDList$Groups$DragStart = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var author$project$DnDList$Groups$Position = F2(
	function (x, y) {
		return {ai: x, aj: y};
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$float = _Json_decodeFloat;
var author$project$Utils$pageX = A2(elm$json$Json$Decode$field, 'pageX', elm$json$Json$Decode$float);
var author$project$Utils$pageY = A2(elm$json$Json$Decode$field, 'pageY', elm$json$Json$Decode$float);
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var author$project$DnDList$Groups$dragEvents = F3(
	function (wrap, dragIndex, sourceElementId) {
		return _List_fromArray(
			[
				A2(
				elm$html$Html$Events$preventDefaultOn,
				'mousedown',
				A2(
					elm$json$Json$Decode$map,
					function (msg) {
						return _Utils_Tuple2(msg, true);
					},
					A2(
						elm$json$Json$Decode$map,
						A2(
							elm$core$Basics$composeL,
							wrap,
							A2(author$project$DnDList$Groups$DragStart, dragIndex, sourceElementId)),
						A3(elm$json$Json$Decode$map2, author$project$DnDList$Groups$Position, author$project$Utils$pageX, author$project$Utils$pageY))))
			]);
	});
var author$project$Utils$px = function (n) {
	return elm$core$String$fromInt(n) + 'px';
};
var author$project$Utils$translate = F2(
	function (x, y) {
		return 'translate3d(' + (author$project$Utils$px(x) + (', ' + (author$project$Utils$px(y) + ', 0)')));
	});
var elm$core$Basics$round = _Basics_round;
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$DnDList$Groups$draggedStyles = F2(
	function (movement, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return _List_Nil;
		} else {
			var m = model.a;
			var _n2 = m.af;
			if (!_n2.$) {
				var element = _n2.a.el;
				switch (movement) {
					case 1:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round((m.am.ai - m.a6.ai) + element.ai),
									elm$core$Basics$round(element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
					case 2:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round(element.ai),
									elm$core$Basics$round((m.am.aj - m.a6.aj) + element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
					default:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round((m.am.ai - m.a6.ai) + element.ai),
									elm$core$Basics$round((m.am.aj - m.a6.aj) + element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
				}
			} else {
				return _List_Nil;
			}
		}
	});
var author$project$DnDList$Groups$DragEnter = function (a) {
	return {$: 3, a: a};
};
var author$project$DnDList$Groups$DragLeave = {$: 4};
var author$project$DnDList$Groups$DragOver = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseenter',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseleave',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$onMouseOver = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseover',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$DnDList$Groups$dropEvents = F3(
	function (wrap, dropIndex, targetElementId) {
		return _List_fromArray(
			[
				elm$html$Html$Events$onMouseOver(
				wrap(
					A2(author$project$DnDList$Groups$DragOver, dropIndex, targetElementId))),
				elm$html$Html$Events$onMouseEnter(
				wrap(
					author$project$DnDList$Groups$DragEnter(dropIndex))),
				elm$html$Html$Events$onMouseLeave(
				wrap(author$project$DnDList$Groups$DragLeave))
			]);
	});
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var author$project$DnDList$Groups$info = function (_n0) {
	var model = _n0;
	return A2(
		elm$core$Maybe$andThen,
		function (m) {
			return A3(
				elm$core$Maybe$map2,
				F2(
					function (source, target) {
						return {cM: m.cM, ek: m.ek, af: source, a5: m.a5, ay: target, az: m.az};
					}),
				m.af,
				m.ay);
		},
		model);
};
var author$project$DnDList$Groups$Drag = function (a) {
	return {$: 1, a: a};
};
var author$project$DnDList$Groups$DragEnd = {$: 5};
var elm$browser$Browser$Events$Document = 0;
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {de: pids, dr: subs};
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {cQ: event, ca: key};
	});
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$core$Process$kill = _Scheduler_kill;
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.de,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.ca;
		var event = _n0.cQ;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.dr);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$browser$Browser$Events$onMouseMove = A2(elm$browser$Browser$Events$on, 0, 'mousemove');
var elm$browser$Browser$Events$onMouseUp = A2(elm$browser$Browser$Events$on, 0, 'mouseup');
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$DnDList$Groups$subscriptions = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Sub$none;
		} else {
			return elm$core$Platform$Sub$batch(
				_List_fromArray(
					[
						elm$browser$Browser$Events$onMouseMove(
						A2(
							elm$json$Json$Decode$map,
							A2(elm$core$Basics$composeL, wrap, author$project$DnDList$Groups$Drag),
							A3(elm$json$Json$Decode$map2, author$project$DnDList$Groups$Position, author$project$Utils$pageX, author$project$Utils$pageY))),
						elm$browser$Browser$Events$onMouseUp(
						elm$json$Json$Decode$succeed(
							wrap(author$project$DnDList$Groups$DragEnd)))
					]));
		}
	});
var author$project$DnDList$Groups$equalGroups = F4(
	function (comparator, dragIndex, dropIndex, list) {
		var dropItem = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		var dragItem = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dragIndex, list));
		var result = A3(
			elm$core$List$map2,
			F2(
				function (dragElement, dropElement) {
					return A2(comparator, dragElement, dropElement);
				}),
			dragItem,
			dropItem);
		return A3(elm$core$List$foldl, elm$core$Basics$or, false, result);
	});
var author$project$Operations$splitAt = F2(
	function (i, list) {
		return _Utils_Tuple2(
			A2(elm$core$List$take, i, list),
			A2(elm$core$List$drop, i, list));
	});
var author$project$Operations$afterBackward = F4(
	function (fn, i, j, list) {
		var _n0 = A2(
			author$project$Operations$splitAt,
			j + 1,
			A3(fn, i, j, list));
		var beginning = _n0.a;
		var rest = _n0.b;
		var _n1 = A2(author$project$Operations$splitAt, (i - j) - 1, rest);
		var middle = _n1.a;
		var end = _n1.b;
		var _n2 = A2(author$project$Operations$splitAt, 1, end);
		var head = _n2.a;
		var tail = _n2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				head,
				_Utils_ap(middle, tail)));
	});
var author$project$Operations$afterForward = F4(
	function (fn, i, j, list) {
		var _n0 = A2(
			author$project$Operations$splitAt,
			i,
			A3(fn, i, j, list));
		var beginning = _n0.a;
		var rest = _n0.b;
		var _n1 = A2(author$project$Operations$splitAt, (j - i) + 1, rest);
		var middle = _n1.a;
		var end = _n1.b;
		var _n2 = A2(author$project$Operations$splitAt, 1, middle);
		var head = _n2.a;
		var tail = _n2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				tail,
				_Utils_ap(head, end)));
	});
var author$project$Operations$insertAfter = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A4(author$project$Operations$afterForward, beforeUpdate, dragIndex, dropIndex, list) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A4(author$project$Operations$afterBackward, beforeUpdate, dragIndex, dropIndex, list) : list);
	});
var author$project$Operations$beforeBackward = F4(
	function (fn, i, j, list) {
		var _n0 = A2(
			author$project$Operations$splitAt,
			j,
			A3(fn, i, j, list));
		var beginning = _n0.a;
		var rest = _n0.b;
		var _n1 = A2(author$project$Operations$splitAt, i - j, rest);
		var middle = _n1.a;
		var end = _n1.b;
		var _n2 = A2(author$project$Operations$splitAt, 1, end);
		var head = _n2.a;
		var tail = _n2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				head,
				_Utils_ap(middle, tail)));
	});
var author$project$Operations$beforeForward = F4(
	function (fn, i, j, list) {
		var _n0 = A2(
			author$project$Operations$splitAt,
			i,
			A3(fn, i, j, list));
		var beginning = _n0.a;
		var rest = _n0.b;
		var _n1 = A2(author$project$Operations$splitAt, j - i, rest);
		var middle = _n1.a;
		var end = _n1.b;
		var _n2 = A2(author$project$Operations$splitAt, 1, middle);
		var head = _n2.a;
		var tail = _n2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				tail,
				_Utils_ap(head, end)));
	});
var author$project$Operations$insertBefore = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A4(author$project$Operations$beforeForward, beforeUpdate, dragIndex, dropIndex, list) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A4(author$project$Operations$beforeBackward, beforeUpdate, dragIndex, dropIndex, list) : list);
	});
var author$project$Operations$rotateIn = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A4(author$project$Operations$beforeForward, beforeUpdate, dragIndex, dropIndex, list) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A4(author$project$Operations$afterBackward, beforeUpdate, dragIndex, dropIndex, list) : list);
	});
var author$project$Operations$rotateOut = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A4(author$project$Operations$afterForward, beforeUpdate, dragIndex, dropIndex, list) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A4(author$project$Operations$beforeBackward, beforeUpdate, dragIndex, dropIndex, list) : list);
	});
var author$project$Operations$swapAt = F3(
	function (i, j, list) {
		var item_j = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, j, list));
		var item_i = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, i, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, i) ? item_j : (_Utils_eq(index, j) ? item_i : _List_fromArray(
							[item]));
					}),
				list));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var author$project$Operations$swap = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A3(
			author$project$Operations$swapAt,
			dragIndex,
			dropIndex,
			A3(beforeUpdate, dragIndex, dropIndex, list)) : list;
	});
var author$project$Operations$unmove = F4(
	function (beforeUpdate, dragIndex, dropIndex, list) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A3(beforeUpdate, dragIndex, dropIndex, list) : list;
	});
var author$project$DnDList$Groups$onDragUpdate = F5(
	function (dropIndex, m, operation, beforeUpdate, list) {
		switch (operation) {
			case 0:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) > 0) ? (dropIndex + 1) : dropIndex
							})),
					A4(author$project$Operations$insertAfter, beforeUpdate, m.cM, dropIndex, list));
			case 1:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) < 0) ? (dropIndex - 1) : dropIndex
							})),
					A4(author$project$Operations$insertBefore, beforeUpdate, m.cM, dropIndex, list));
			case 2:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) < 0) ? (dropIndex - 1) : ((_Utils_cmp(m.cM, dropIndex) > 0) ? (dropIndex + 1) : dropIndex)
							})),
					A4(author$project$Operations$rotateIn, beforeUpdate, m.cM, dropIndex, list));
			case 3:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0, cM: dropIndex})),
					A4(author$project$Operations$rotateOut, beforeUpdate, m.cM, dropIndex, list));
			case 4:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0, cM: dropIndex})),
					A4(author$project$Operations$swap, beforeUpdate, m.cM, dropIndex, list));
			default:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0})),
					A4(author$project$Operations$unmove, beforeUpdate, m.cM, dropIndex, list));
		}
	});
var author$project$DnDList$Groups$onDropUpdate = F4(
	function (m, operation, beforeUpdate, list) {
		switch (operation) {
			case 0:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$insertAfter, beforeUpdate, m.cM, m.ek, list));
			case 1:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$insertBefore, beforeUpdate, m.cM, m.ek, list));
			case 2:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$rotateIn, beforeUpdate, m.cM, m.ek, list));
			case 3:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$rotateOut, beforeUpdate, m.cM, m.ek, list));
			case 4:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$swap, beforeUpdate, m.cM, m.ek, list));
			default:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$unmove, beforeUpdate, m.cM, m.ek, list));
		}
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$DnDList$Groups$update = F4(
	function (_n0, msg, _n1, list) {
		var operation = _n0.c8;
		var trigger = _n0.fr;
		var beforeUpdate = _n0.cB;
		var groups = _n0.es;
		var model = _n1;
		switch (msg.$) {
			case 0:
				var dragIndex = msg.a;
				var sourceElementId = msg.b;
				var xy = msg.c;
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						{am: xy, m: 0, cM: dragIndex, ek: dragIndex, af: elm$core$Maybe$Nothing, a5: sourceElementId, a6: xy, ay: elm$core$Maybe$Nothing, az: sourceElementId}),
					list);
			case 1:
				var xy = msg.a;
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{am: xy, m: m.m + 1});
						},
						model),
					list);
			case 2:
				var dropIndex = msg.a;
				var targetElementId = msg.b;
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{ek: dropIndex, az: targetElementId});
						},
						model),
					list);
			case 3:
				var dropIndex = msg.a;
				var _n3 = _Utils_Tuple2(model, trigger);
				if ((!_n3.a.$) && (!_n3.b)) {
					var m = _n3.a.a;
					var _n4 = _n3.b;
					return ((m.m > 1) && (!_Utils_eq(m.cM, dropIndex))) ? (A4(author$project$DnDList$Groups$equalGroups, groups.eb, m.cM, dropIndex, list) ? A5(author$project$DnDList$Groups$onDragUpdate, dropIndex, m, operation, beforeUpdate, list) : A5(author$project$DnDList$Groups$onDragUpdate, dropIndex, m, groups.c8, groups.cB, list)) : _Utils_Tuple2(model, list);
				} else {
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{m: 0});
							},
							model),
						list);
				}
			case 4:
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{ek: m.cM});
						},
						model),
					list);
			case 5:
				var _n5 = _Utils_Tuple2(model, trigger);
				if ((!_n5.a.$) && (_n5.b === 1)) {
					var m = _n5.a.a;
					var _n6 = _n5.b;
					return (!_Utils_eq(m.cM, m.ek)) ? (A4(author$project$DnDList$Groups$equalGroups, groups.eb, m.cM, m.ek, list) ? A4(author$project$DnDList$Groups$onDropUpdate, m, operation, beforeUpdate, list) : A4(author$project$DnDList$Groups$onDropUpdate, m, groups.c8, groups.cB, list)) : _Utils_Tuple2(elm$core$Maybe$Nothing, list);
				} else {
					return _Utils_Tuple2(elm$core$Maybe$Nothing, list);
				}
			case 6:
				if (msg.a.$ === 1) {
					return _Utils_Tuple2(model, list);
				} else {
					var sourceElement = msg.a.a;
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{
										af: elm$core$Maybe$Just(sourceElement),
										ay: elm$core$Maybe$Just(sourceElement)
									});
							},
							model),
						list);
				}
			default:
				if (msg.a.$ === 1) {
					return _Utils_Tuple2(model, list);
				} else {
					var targetElement = msg.a.a;
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{
										ay: elm$core$Maybe$Just(targetElement)
									});
							},
							model),
						list);
				}
		}
	});
var author$project$DnDList$Groups$create = F2(
	function (config, message) {
		return {
			ea: author$project$DnDList$Groups$commands(message),
			ei: author$project$DnDList$Groups$dragEvents(message),
			z: elm$core$Maybe$Nothing,
			ej: author$project$DnDList$Groups$draggedStyles(config.eI),
			cN: author$project$DnDList$Groups$dropEvents(message),
			c_: author$project$DnDList$Groups$info,
			ds: author$project$DnDList$Groups$subscriptions(message),
			dz: author$project$DnDList$Groups$update(config)
		};
	});
var author$project$Configuration$Groups$InsertAfter$system = A2(author$project$DnDList$Groups$create, author$project$Configuration$Groups$InsertAfter$config, elm$core$Basics$identity);
var author$project$Configuration$Groups$InsertAfter$initialModel = {z: author$project$Configuration$Groups$InsertAfter$system.z, A: author$project$Configuration$Groups$InsertAfter$gatheredByGroup};
var author$project$Configuration$Groups$InsertBefore$Item = F3(
	function (group, value, color) {
		return {b3: color, t: group, co: value};
	});
var author$project$Configuration$Groups$InsertBefore$blue = '#45858c';
var author$project$Configuration$Groups$InsertBefore$green = '#858c45';
var author$project$Configuration$Groups$InsertBefore$red = '#8c4585';
var author$project$Configuration$Groups$InsertBefore$transparent = 'transparent';
var author$project$Configuration$Groups$InsertBefore$gatheredByGroup = _List_fromArray(
	[
		A3(author$project$Configuration$Groups$InsertBefore$Item, 0, '2', author$project$Configuration$Groups$InsertBefore$red),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 0, 'B', author$project$Configuration$Groups$InsertBefore$blue),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 0, 'A', author$project$Configuration$Groups$InsertBefore$blue),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 0, 'III', author$project$Configuration$Groups$InsertBefore$green),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 0, '', author$project$Configuration$Groups$InsertBefore$transparent),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 1, 'C', author$project$Configuration$Groups$InsertBefore$blue),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 1, '1', author$project$Configuration$Groups$InsertBefore$red),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 1, 'I', author$project$Configuration$Groups$InsertBefore$green),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 1, '3', author$project$Configuration$Groups$InsertBefore$red),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 1, '', author$project$Configuration$Groups$InsertBefore$transparent),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 2, 'II', author$project$Configuration$Groups$InsertBefore$green),
		A3(author$project$Configuration$Groups$InsertBefore$Item, 2, '', author$project$Configuration$Groups$InsertBefore$transparent)
	]);
var author$project$Configuration$Groups$InsertBefore$MyMsg = elm$core$Basics$identity;
var author$project$Configuration$Groups$InsertBefore$compareByGroup = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.t, dropItem.t);
	});
var author$project$Configuration$Groups$InsertBefore$updateOnGroupChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{t: dropItem.t});
								}),
							_List_fromArray(
								[item]),
							drop) : _List_fromArray(
							[item]);
					}),
				list));
	});
var author$project$DnDList$Groups$InsertBefore = 1;
var author$project$Configuration$Groups$InsertBefore$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Configuration$Groups$InsertBefore$updateOnGroupChange, eb: author$project$Configuration$Groups$InsertBefore$compareByGroup, c8: 1},
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Configuration$Groups$InsertBefore$system = A2(author$project$DnDList$Groups$create, author$project$Configuration$Groups$InsertBefore$config, elm$core$Basics$identity);
var author$project$Configuration$Groups$InsertBefore$initialModel = {z: author$project$Configuration$Groups$InsertBefore$system.z, A: author$project$Configuration$Groups$InsertBefore$gatheredByGroup};
var author$project$Configuration$Groups$Root$InsertAfter = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Groups$Root$InsertBefore = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Groups$Root$Swap = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Groups$Swap$Item = F3(
	function (group, value, color) {
		return {b3: color, t: group, co: value};
	});
var author$project$Configuration$Groups$Swap$blue = '#45858c';
var author$project$Configuration$Groups$Swap$green = '#858c45';
var author$project$Configuration$Groups$Swap$red = '#8c4585';
var author$project$Configuration$Groups$Swap$gatheredByGroup = _List_fromArray(
	[
		A3(author$project$Configuration$Groups$Swap$Item, 0, '2', author$project$Configuration$Groups$Swap$red),
		A3(author$project$Configuration$Groups$Swap$Item, 0, 'B', author$project$Configuration$Groups$Swap$blue),
		A3(author$project$Configuration$Groups$Swap$Item, 0, 'III', author$project$Configuration$Groups$Swap$green),
		A3(author$project$Configuration$Groups$Swap$Item, 1, 'C', author$project$Configuration$Groups$Swap$blue),
		A3(author$project$Configuration$Groups$Swap$Item, 1, '1', author$project$Configuration$Groups$Swap$red),
		A3(author$project$Configuration$Groups$Swap$Item, 1, 'I', author$project$Configuration$Groups$Swap$green),
		A3(author$project$Configuration$Groups$Swap$Item, 2, '3', author$project$Configuration$Groups$Swap$red),
		A3(author$project$Configuration$Groups$Swap$Item, 2, 'II', author$project$Configuration$Groups$Swap$green),
		A3(author$project$Configuration$Groups$Swap$Item, 2, 'A', author$project$Configuration$Groups$Swap$blue)
	]);
var author$project$Configuration$Groups$Swap$MyMsg = elm$core$Basics$identity;
var author$project$Configuration$Groups$Swap$compareByGroup = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.t, dropItem.t);
	});
var author$project$Configuration$Groups$Swap$updateOnGroupChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		var drag = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dragIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{t: dropItem.t});
								}),
							_List_fromArray(
								[item]),
							drop) : (_Utils_eq(index, dropIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dropItem,
										{t: dragItem.t});
								}),
							drag,
							_List_fromArray(
								[item])) : _List_fromArray(
							[item]));
					}),
				list));
	});
var author$project$Configuration$Groups$Swap$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Configuration$Groups$Swap$updateOnGroupChange, eb: author$project$Configuration$Groups$Swap$compareByGroup, c8: 4},
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Configuration$Groups$Swap$system = A2(author$project$DnDList$Groups$create, author$project$Configuration$Groups$Swap$config, elm$core$Basics$identity);
var author$project$Configuration$Groups$Swap$initialModel = {z: author$project$Configuration$Groups$Swap$system.z, A: author$project$Configuration$Groups$Swap$gatheredByGroup};
var author$project$Configuration$Groups$Root$initialModel = {
	aP: _List_fromArray(
		[
			author$project$Configuration$Groups$Root$InsertAfter(author$project$Configuration$Groups$InsertAfter$initialModel),
			author$project$Configuration$Groups$Root$InsertBefore(author$project$Configuration$Groups$InsertBefore$initialModel),
			author$project$Configuration$Groups$Root$Swap(author$project$Configuration$Groups$Swap$initialModel)
		]),
	bI: 0
};
var author$project$Configuration$Movement$FreeOnDrag$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 9));
var author$project$Configuration$Movement$FreeOnDrag$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$DnDList$Free = 0;
var author$project$DnDList$OnDrag = 0;
var author$project$DnDList$Swap = 4;
var author$project$Configuration$Movement$FreeOnDrag$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$DnDList$Draggable = elm$core$Basics$identity;
var author$project$DnDList$GotSourceElement = function (a) {
	return {$: 6, a: a};
};
var author$project$DnDList$sourceCommands = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Cmd$none;
		} else {
			var m = model.a;
			var _n2 = m.af;
			if (_n2.$ === 1) {
				return A2(
					elm$core$Task$attempt,
					A2(elm$core$Basics$composeL, wrap, author$project$DnDList$GotSourceElement),
					elm$browser$Browser$Dom$getElement(m.a5));
			} else {
				return elm$core$Platform$Cmd$none;
			}
		}
	});
var author$project$DnDList$GotTargetElement = function (a) {
	return {$: 7, a: a};
};
var author$project$DnDList$targetCommands = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Cmd$none;
		} else {
			var m = model.a;
			return (!m.m) ? A2(
				elm$core$Task$attempt,
				A2(elm$core$Basics$composeL, wrap, author$project$DnDList$GotTargetElement),
				elm$browser$Browser$Dom$getElement(m.az)) : elm$core$Platform$Cmd$none;
		}
	});
var author$project$DnDList$commands = F2(
	function (wrap, draggable) {
		return elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2(author$project$DnDList$sourceCommands, wrap, draggable),
					A2(author$project$DnDList$targetCommands, wrap, draggable)
				]));
	});
var author$project$DnDList$DragStart = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var author$project$DnDList$Position = F2(
	function (x, y) {
		return {ai: x, aj: y};
	});
var author$project$DnDList$dragEvents = F3(
	function (wrap, dragIndex, sourceElementId) {
		return _List_fromArray(
			[
				A2(
				elm$html$Html$Events$preventDefaultOn,
				'mousedown',
				A2(
					elm$json$Json$Decode$map,
					function (msg) {
						return _Utils_Tuple2(msg, true);
					},
					A2(
						elm$json$Json$Decode$map,
						A2(
							elm$core$Basics$composeL,
							wrap,
							A2(author$project$DnDList$DragStart, dragIndex, sourceElementId)),
						A3(elm$json$Json$Decode$map2, author$project$DnDList$Position, author$project$Utils$pageX, author$project$Utils$pageY))))
			]);
	});
var author$project$DnDList$draggedStyles = F2(
	function (movement, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return _List_Nil;
		} else {
			var m = model.a;
			var _n2 = m.af;
			if (!_n2.$) {
				var element = _n2.a.el;
				switch (movement) {
					case 1:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round((m.am.ai - m.a6.ai) + element.ai),
									elm$core$Basics$round(element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
					case 2:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round(element.ai),
									elm$core$Basics$round((m.am.aj - m.a6.aj) + element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
					default:
						return _List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
								A2(elm$html$Html$Attributes$style, 'left', '0'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(
								elm$html$Html$Attributes$style,
								'transform',
								A2(
									author$project$Utils$translate,
									elm$core$Basics$round((m.am.ai - m.a6.ai) + element.ai),
									elm$core$Basics$round((m.am.aj - m.a6.aj) + element.aj))),
								A2(
								elm$html$Html$Attributes$style,
								'height',
								author$project$Utils$px(
									elm$core$Basics$round(element.b8))),
								A2(
								elm$html$Html$Attributes$style,
								'width',
								author$project$Utils$px(
									elm$core$Basics$round(element.cp))),
								A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
							]);
				}
			} else {
				return _List_Nil;
			}
		}
	});
var author$project$DnDList$DragEnter = function (a) {
	return {$: 3, a: a};
};
var author$project$DnDList$DragLeave = {$: 4};
var author$project$DnDList$DragOver = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var author$project$DnDList$dropEvents = F3(
	function (wrap, dropIndex, targetElementId) {
		return _List_fromArray(
			[
				elm$html$Html$Events$onMouseOver(
				wrap(
					A2(author$project$DnDList$DragOver, dropIndex, targetElementId))),
				elm$html$Html$Events$onMouseEnter(
				wrap(
					author$project$DnDList$DragEnter(dropIndex))),
				elm$html$Html$Events$onMouseLeave(
				wrap(author$project$DnDList$DragLeave))
			]);
	});
var author$project$DnDList$info = function (_n0) {
	var model = _n0;
	return A2(
		elm$core$Maybe$andThen,
		function (m) {
			return A3(
				elm$core$Maybe$map2,
				F2(
					function (source, target) {
						return {cM: m.cM, ek: m.ek, af: source, a5: m.a5, ay: target, az: m.az};
					}),
				m.af,
				m.ay);
		},
		model);
};
var author$project$DnDList$Drag = function (a) {
	return {$: 1, a: a};
};
var author$project$DnDList$DragEnd = {$: 5};
var author$project$DnDList$subscriptions = F2(
	function (wrap, _n0) {
		var model = _n0;
		if (model.$ === 1) {
			return elm$core$Platform$Sub$none;
		} else {
			return elm$core$Platform$Sub$batch(
				_List_fromArray(
					[
						elm$browser$Browser$Events$onMouseMove(
						A2(
							elm$json$Json$Decode$map,
							A2(elm$core$Basics$composeL, wrap, author$project$DnDList$Drag),
							A3(elm$json$Json$Decode$map2, author$project$DnDList$Position, author$project$Utils$pageX, author$project$Utils$pageY))),
						elm$browser$Browser$Events$onMouseUp(
						elm$json$Json$Decode$succeed(
							wrap(author$project$DnDList$DragEnd)))
					]));
		}
	});
var author$project$DnDList$onDragUpdate = F5(
	function (dropIndex, m, operation, beforeUpdate, list) {
		switch (operation) {
			case 0:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) > 0) ? (dropIndex + 1) : dropIndex
							})),
					A4(author$project$Operations$insertAfter, beforeUpdate, m.cM, dropIndex, list));
			case 1:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) < 0) ? (dropIndex - 1) : dropIndex
							})),
					A4(author$project$Operations$insertBefore, beforeUpdate, m.cM, dropIndex, list));
			case 2:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{
								m: 0,
								cM: (_Utils_cmp(m.cM, dropIndex) < 0) ? (dropIndex - 1) : ((_Utils_cmp(m.cM, dropIndex) > 0) ? (dropIndex + 1) : dropIndex)
							})),
					A4(author$project$Operations$rotateIn, beforeUpdate, m.cM, dropIndex, list));
			case 3:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0, cM: dropIndex})),
					A4(author$project$Operations$rotateOut, beforeUpdate, m.cM, dropIndex, list));
			case 4:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0, cM: dropIndex})),
					A4(author$project$Operations$swap, beforeUpdate, m.cM, dropIndex, list));
			default:
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						_Utils_update(
							m,
							{m: 0})),
					A4(author$project$Operations$unmove, beforeUpdate, m.cM, dropIndex, list));
		}
	});
var author$project$DnDList$onDropUpdate = F4(
	function (m, operation, beforeUpdate, list) {
		switch (operation) {
			case 0:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$insertAfter, beforeUpdate, m.cM, m.ek, list));
			case 1:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$insertBefore, beforeUpdate, m.cM, m.ek, list));
			case 2:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$rotateIn, beforeUpdate, m.cM, m.ek, list));
			case 3:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$rotateOut, beforeUpdate, m.cM, m.ek, list));
			case 4:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$swap, beforeUpdate, m.cM, m.ek, list));
			default:
				return _Utils_Tuple2(
					elm$core$Maybe$Nothing,
					A4(author$project$Operations$unmove, beforeUpdate, m.cM, m.ek, list));
		}
	});
var author$project$DnDList$update = F4(
	function (_n0, msg, _n1, list) {
		var operation = _n0.c8;
		var trigger = _n0.fr;
		var beforeUpdate = _n0.cB;
		var model = _n1;
		switch (msg.$) {
			case 0:
				var dragIndex = msg.a;
				var sourceElementId = msg.b;
				var xy = msg.c;
				return _Utils_Tuple2(
					elm$core$Maybe$Just(
						{am: xy, m: 0, cM: dragIndex, ek: dragIndex, af: elm$core$Maybe$Nothing, a5: sourceElementId, a6: xy, ay: elm$core$Maybe$Nothing, az: sourceElementId}),
					list);
			case 1:
				var xy = msg.a;
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{am: xy, m: m.m + 1});
						},
						model),
					list);
			case 2:
				var dropIndex = msg.a;
				var targetElementId = msg.b;
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{ek: dropIndex, az: targetElementId});
						},
						model),
					list);
			case 3:
				var dropIndex = msg.a;
				var _n3 = _Utils_Tuple2(model, trigger);
				if ((!_n3.a.$) && (!_n3.b)) {
					var m = _n3.a.a;
					var _n4 = _n3.b;
					return ((m.m > 1) && (!_Utils_eq(m.cM, dropIndex))) ? A5(author$project$DnDList$onDragUpdate, dropIndex, m, operation, beforeUpdate, list) : _Utils_Tuple2(model, list);
				} else {
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{m: 0});
							},
							model),
						list);
				}
			case 4:
				return _Utils_Tuple2(
					A2(
						elm$core$Maybe$map,
						function (m) {
							return _Utils_update(
								m,
								{ek: m.cM});
						},
						model),
					list);
			case 5:
				var _n5 = _Utils_Tuple2(model, trigger);
				if ((!_n5.a.$) && (_n5.b === 1)) {
					var m = _n5.a.a;
					var _n6 = _n5.b;
					return (!_Utils_eq(m.cM, m.ek)) ? A4(author$project$DnDList$onDropUpdate, m, operation, beforeUpdate, list) : _Utils_Tuple2(elm$core$Maybe$Nothing, list);
				} else {
					return _Utils_Tuple2(elm$core$Maybe$Nothing, list);
				}
			case 6:
				if (msg.a.$ === 1) {
					return _Utils_Tuple2(model, list);
				} else {
					var sourceElement = msg.a.a;
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{
										af: elm$core$Maybe$Just(sourceElement),
										ay: elm$core$Maybe$Just(sourceElement)
									});
							},
							model),
						list);
				}
			default:
				if (msg.a.$ === 1) {
					return _Utils_Tuple2(model, list);
				} else {
					var targetElement = msg.a.a;
					return _Utils_Tuple2(
						A2(
							elm$core$Maybe$map,
							function (m) {
								return _Utils_update(
									m,
									{
										ay: elm$core$Maybe$Just(targetElement)
									});
							},
							model),
						list);
				}
		}
	});
var author$project$DnDList$create = F2(
	function (config, message) {
		return {
			ea: author$project$DnDList$commands(message),
			ei: author$project$DnDList$dragEvents(message),
			z: elm$core$Maybe$Nothing,
			ej: author$project$DnDList$draggedStyles(config.eI),
			cN: author$project$DnDList$dropEvents(message),
			c_: author$project$DnDList$info,
			ds: author$project$DnDList$subscriptions(message),
			dz: author$project$DnDList$update(config)
		};
	});
var author$project$Configuration$Movement$FreeOnDrag$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$FreeOnDrag$config, author$project$Configuration$Movement$FreeOnDrag$MyMsg);
var author$project$Configuration$Movement$FreeOnDrag$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$FreeOnDrag$system.z, A: author$project$Configuration$Movement$FreeOnDrag$data};
var author$project$Configuration$Movement$FreeOnDrop$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 9));
var author$project$Configuration$Movement$FreeOnDrop$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$DnDList$OnDrop = 1;
var author$project$Configuration$Movement$FreeOnDrop$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 1
};
var author$project$Configuration$Movement$FreeOnDrop$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$FreeOnDrop$config, author$project$Configuration$Movement$FreeOnDrop$MyMsg);
var author$project$Configuration$Movement$FreeOnDrop$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$FreeOnDrop$system.z, A: author$project$Configuration$Movement$FreeOnDrop$data};
var author$project$Configuration$Movement$HorizontalOnDrag$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 7));
var author$project$Configuration$Movement$HorizontalOnDrag$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$DnDList$Horizontal = 1;
var author$project$Configuration$Movement$HorizontalOnDrag$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 1,
	c8: 4,
	fr: 0
};
var author$project$Configuration$Movement$HorizontalOnDrag$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$HorizontalOnDrag$config, author$project$Configuration$Movement$HorizontalOnDrag$MyMsg);
var author$project$Configuration$Movement$HorizontalOnDrag$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$HorizontalOnDrag$system.z, A: author$project$Configuration$Movement$HorizontalOnDrag$data};
var author$project$Configuration$Movement$HorizontalOnDrop$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 7));
var author$project$Configuration$Movement$HorizontalOnDrop$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Movement$HorizontalOnDrop$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 1,
	c8: 4,
	fr: 1
};
var author$project$Configuration$Movement$HorizontalOnDrop$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$HorizontalOnDrop$config, author$project$Configuration$Movement$HorizontalOnDrop$MyMsg);
var author$project$Configuration$Movement$HorizontalOnDrop$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$HorizontalOnDrop$system.z, A: author$project$Configuration$Movement$HorizontalOnDrop$data};
var author$project$Configuration$Movement$Root$FreeOnDrag = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Movement$Root$FreeOnDrop = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Movement$Root$HorizontalOnDrag = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Movement$Root$HorizontalOnDrop = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$Movement$Root$VerticalOnDrag = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$Movement$Root$VerticalOnDrop = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$Movement$VerticalOnDrag$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 7));
var author$project$Configuration$Movement$VerticalOnDrag$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$DnDList$Vertical = 2;
var author$project$Configuration$Movement$VerticalOnDrag$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 2,
	c8: 4,
	fr: 0
};
var author$project$Configuration$Movement$VerticalOnDrag$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$VerticalOnDrag$config, author$project$Configuration$Movement$VerticalOnDrag$MyMsg);
var author$project$Configuration$Movement$VerticalOnDrag$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$VerticalOnDrag$system.z, A: author$project$Configuration$Movement$VerticalOnDrag$data};
var author$project$Configuration$Movement$VerticalOnDrop$data = A2(
	elm$core$List$map,
	elm$core$String$fromInt,
	A2(elm$core$List$range, 1, 7));
var author$project$Configuration$Movement$VerticalOnDrop$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Movement$VerticalOnDrop$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 2,
	c8: 4,
	fr: 1
};
var author$project$Configuration$Movement$VerticalOnDrop$system = A2(author$project$DnDList$create, author$project$Configuration$Movement$VerticalOnDrop$config, author$project$Configuration$Movement$VerticalOnDrop$MyMsg);
var author$project$Configuration$Movement$VerticalOnDrop$initialModel = {O: _List_Nil, z: author$project$Configuration$Movement$VerticalOnDrop$system.z, A: author$project$Configuration$Movement$VerticalOnDrop$data};
var author$project$Configuration$Movement$Root$initialModel = {
	aP: _List_fromArray(
		[
			author$project$Configuration$Movement$Root$FreeOnDrag(author$project$Configuration$Movement$FreeOnDrag$initialModel),
			author$project$Configuration$Movement$Root$FreeOnDrop(author$project$Configuration$Movement$FreeOnDrop$initialModel),
			author$project$Configuration$Movement$Root$HorizontalOnDrag(author$project$Configuration$Movement$HorizontalOnDrag$initialModel),
			author$project$Configuration$Movement$Root$HorizontalOnDrop(author$project$Configuration$Movement$HorizontalOnDrop$initialModel),
			author$project$Configuration$Movement$Root$VerticalOnDrag(author$project$Configuration$Movement$VerticalOnDrag$initialModel),
			author$project$Configuration$Movement$Root$VerticalOnDrop(author$project$Configuration$Movement$VerticalOnDrop$initialModel)
		]),
	bI: 0
};
var author$project$Configuration$OperationOnDrag$InsertAfter$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$InsertAfter$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$InsertAfter$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$InsertAfter$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$InsertAfter$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$InsertAfter$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$InsertAfter$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrag$InsertAfter$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$InsertAfter$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$InsertAfter$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$affectedColor) : A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, color)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					var color = _n1.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$affectedColor) : A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, color)));
				}),
			items) : items);
	});
var author$project$DnDList$InsertAfter = 0;
var author$project$Configuration$OperationOnDrag$InsertAfter$config = {cB: author$project$Configuration$OperationOnDrag$InsertAfter$updateColors, eI: 0, c8: 0, fr: 0};
var author$project$Configuration$OperationOnDrag$InsertAfter$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$InsertAfter$config, author$project$Configuration$OperationOnDrag$InsertAfter$MyMsg);
var author$project$Configuration$OperationOnDrag$InsertAfter$initialModel = {z: author$project$Configuration$OperationOnDrag$InsertAfter$system.z, A: author$project$Configuration$OperationOnDrag$InsertAfter$data};
var author$project$Configuration$OperationOnDrag$InsertBefore$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$InsertBefore$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$InsertBefore$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$InsertBefore$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$InsertBefore$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$InsertBefore$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$InsertBefore$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrag$InsertBefore$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$InsertBefore$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$InsertBefore$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$affectedColor) : A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, color)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					var color = _n1.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$affectedColor) : A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, color)));
				}),
			items) : items);
	});
var author$project$DnDList$InsertBefore = 1;
var author$project$Configuration$OperationOnDrag$InsertBefore$config = {cB: author$project$Configuration$OperationOnDrag$InsertBefore$updateColors, eI: 0, c8: 1, fr: 0};
var author$project$Configuration$OperationOnDrag$InsertBefore$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$InsertBefore$config, author$project$Configuration$OperationOnDrag$InsertBefore$MyMsg);
var author$project$Configuration$OperationOnDrag$InsertBefore$initialModel = {z: author$project$Configuration$OperationOnDrag$InsertBefore$system.z, A: author$project$Configuration$OperationOnDrag$InsertBefore$data};
var author$project$Configuration$OperationOnDrag$Root$InsertAfter = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$InsertBefore = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$RotateIn = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$RotateOut = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$Swap = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$Unmove = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$OperationOnDrag$RotateIn$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$RotateIn$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$RotateIn$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$RotateIn$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$RotateIn$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$RotateIn$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$RotateIn$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrag$RotateIn$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$RotateIn$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$RotateIn$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$affectedColor) : A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, color)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					var color = _n1.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$affectedColor) : A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, color)));
				}),
			items) : items);
	});
var author$project$DnDList$RotateIn = 2;
var author$project$Configuration$OperationOnDrag$RotateIn$config = {cB: author$project$Configuration$OperationOnDrag$RotateIn$updateColors, eI: 0, c8: 2, fr: 0};
var author$project$Configuration$OperationOnDrag$RotateIn$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$RotateIn$config, author$project$Configuration$OperationOnDrag$RotateIn$MyMsg);
var author$project$Configuration$OperationOnDrag$RotateIn$initialModel = {z: author$project$Configuration$OperationOnDrag$RotateIn$system.z, A: author$project$Configuration$OperationOnDrag$RotateIn$data};
var author$project$Configuration$OperationOnDrag$RotateOut$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$RotateOut$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$RotateOut$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$RotateOut$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$RotateOut$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$RotateOut$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$RotateOut$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrag$RotateOut$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$RotateOut$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$RotateOut$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$affectedColor) : A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, color)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					var color = _n1.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$affectedColor) : A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, color)));
				}),
			items) : items);
	});
var author$project$DnDList$RotateOut = 3;
var author$project$Configuration$OperationOnDrag$RotateOut$config = {cB: author$project$Configuration$OperationOnDrag$RotateOut$updateColors, eI: 0, c8: 3, fr: 0};
var author$project$Configuration$OperationOnDrag$RotateOut$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$RotateOut$config, author$project$Configuration$OperationOnDrag$RotateOut$MyMsg);
var author$project$Configuration$OperationOnDrag$RotateOut$initialModel = {z: author$project$Configuration$OperationOnDrag$RotateOut$system.z, A: author$project$Configuration$OperationOnDrag$RotateOut$data};
var author$project$Configuration$OperationOnDrag$Swap$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$Swap$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$Swap$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$Swap$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$Swap$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$Swap$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$Swap$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$Swap$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$Swap$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$Swap$Item, value, author$project$Configuration$OperationOnDrag$Swap$sourceColor) : (_Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$Swap$Item, value, author$project$Configuration$OperationOnDrag$Swap$targetColor) : A2(author$project$Configuration$OperationOnDrag$Swap$Item, value, color));
				}),
			items) : items;
	});
var author$project$Configuration$OperationOnDrag$Swap$config = {cB: author$project$Configuration$OperationOnDrag$Swap$updateColors, eI: 0, c8: 4, fr: 0};
var author$project$Configuration$OperationOnDrag$Swap$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$Swap$config, author$project$Configuration$OperationOnDrag$Swap$MyMsg);
var author$project$Configuration$OperationOnDrag$Swap$initialModel = {z: author$project$Configuration$OperationOnDrag$Swap$system.z, A: author$project$Configuration$OperationOnDrag$Swap$data};
var author$project$Configuration$OperationOnDrag$Unmove$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrag$Unmove$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrag$Unmove$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrag$Unmove$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrag$Unmove$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrag$Unmove$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$Unmove$sourceColor = 'green';
var author$project$Configuration$OperationOnDrag$Unmove$targetColor = 'red';
var author$project$Configuration$OperationOnDrag$Unmove$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					var color = _n0.b3;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrag$Unmove$Item, value, author$project$Configuration$OperationOnDrag$Unmove$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrag$Unmove$Item, value, author$project$Configuration$OperationOnDrag$Unmove$sourceColor) : A2(author$project$Configuration$OperationOnDrag$Unmove$Item, value, color));
				}),
			items) : items;
	});
var author$project$DnDList$Unmove = 5;
var author$project$Configuration$OperationOnDrag$Unmove$config = {cB: author$project$Configuration$OperationOnDrag$Unmove$updateColors, eI: 0, c8: 5, fr: 0};
var author$project$Configuration$OperationOnDrag$Unmove$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrag$Unmove$config, author$project$Configuration$OperationOnDrag$Unmove$MyMsg);
var author$project$Configuration$OperationOnDrag$Unmove$initialModel = {z: author$project$Configuration$OperationOnDrag$Unmove$system.z, A: author$project$Configuration$OperationOnDrag$Unmove$data};
var author$project$Configuration$OperationOnDrag$Root$initialModel = {
	aP: _List_fromArray(
		[
			author$project$Configuration$OperationOnDrag$Root$InsertAfter(author$project$Configuration$OperationOnDrag$InsertAfter$initialModel),
			author$project$Configuration$OperationOnDrag$Root$InsertBefore(author$project$Configuration$OperationOnDrag$InsertBefore$initialModel),
			author$project$Configuration$OperationOnDrag$Root$RotateIn(author$project$Configuration$OperationOnDrag$RotateIn$initialModel),
			author$project$Configuration$OperationOnDrag$Root$RotateOut(author$project$Configuration$OperationOnDrag$RotateOut$initialModel),
			author$project$Configuration$OperationOnDrag$Root$Swap(author$project$Configuration$OperationOnDrag$Swap$initialModel),
			author$project$Configuration$OperationOnDrag$Root$Unmove(author$project$Configuration$OperationOnDrag$Unmove$initialModel)
		]),
	bI: 0
};
var author$project$Configuration$OperationOnDrop$InsertAfter$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$InsertAfter$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$InsertAfter$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$InsertAfter$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$InsertAfter$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$InsertAfter$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$InsertAfter$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrop$InsertAfter$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$InsertAfter$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$InsertAfter$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$affectedColor) : A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$baseColor)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$affectedColor) : A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$baseColor)));
				}),
			items) : items);
	});
var author$project$Configuration$OperationOnDrop$InsertAfter$config = {cB: author$project$Configuration$OperationOnDrop$InsertAfter$updateColors, eI: 0, c8: 0, fr: 1};
var author$project$Configuration$OperationOnDrop$InsertAfter$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$InsertAfter$config, author$project$Configuration$OperationOnDrop$InsertAfter$MyMsg);
var author$project$Configuration$OperationOnDrop$InsertAfter$initialModel = {z: author$project$Configuration$OperationOnDrop$InsertAfter$system.z, A: author$project$Configuration$OperationOnDrop$InsertAfter$data};
var author$project$Configuration$OperationOnDrop$InsertBefore$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$InsertBefore$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$InsertBefore$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$InsertBefore$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$InsertBefore$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$InsertBefore$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$InsertBefore$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrop$InsertBefore$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$InsertBefore$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$InsertBefore$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$affectedColor) : A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$baseColor)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$affectedColor) : A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$baseColor)));
				}),
			items) : items);
	});
var author$project$Configuration$OperationOnDrop$InsertBefore$config = {cB: author$project$Configuration$OperationOnDrop$InsertBefore$updateColors, eI: 0, c8: 1, fr: 1};
var author$project$Configuration$OperationOnDrop$InsertBefore$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$InsertBefore$config, author$project$Configuration$OperationOnDrop$InsertBefore$MyMsg);
var author$project$Configuration$OperationOnDrop$InsertBefore$initialModel = {z: author$project$Configuration$OperationOnDrop$InsertBefore$system.z, A: author$project$Configuration$OperationOnDrop$InsertBefore$data};
var author$project$Configuration$OperationOnDrop$Root$InsertAfter = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$InsertBefore = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$RotateIn = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$RotateOut = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$Swap = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$Unmove = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$OperationOnDrop$RotateIn$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$RotateIn$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$RotateIn$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$RotateIn$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$RotateIn$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$RotateIn$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$RotateIn$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrop$RotateIn$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$RotateIn$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$RotateIn$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$affectedColor) : A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$baseColor)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$affectedColor) : A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$baseColor)));
				}),
			items) : items);
	});
var author$project$Configuration$OperationOnDrop$RotateIn$config = {cB: author$project$Configuration$OperationOnDrop$RotateIn$updateColors, eI: 0, c8: 2, fr: 1};
var author$project$Configuration$OperationOnDrop$RotateIn$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$RotateIn$config, author$project$Configuration$OperationOnDrop$RotateIn$MyMsg);
var author$project$Configuration$OperationOnDrop$RotateIn$initialModel = {z: author$project$Configuration$OperationOnDrop$RotateIn$system.z, A: author$project$Configuration$OperationOnDrop$RotateIn$data};
var author$project$Configuration$OperationOnDrop$RotateOut$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$RotateOut$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$RotateOut$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$RotateOut$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$RotateOut$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$RotateOut$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$RotateOut$affectedColor = 'purple';
var author$project$Configuration$OperationOnDrop$RotateOut$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$RotateOut$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$RotateOut$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$sourceColor) : (((_Utils_cmp(dragIndex, i) < 0) && (_Utils_cmp(i, dropIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$affectedColor) : A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$baseColor)));
				}),
			items) : ((_Utils_cmp(dragIndex, dropIndex) > 0) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n1) {
					var value = _n1.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$sourceColor) : (((_Utils_cmp(dropIndex, i) < 0) && (_Utils_cmp(i, dragIndex) < 0)) ? A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$affectedColor) : A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$baseColor)));
				}),
			items) : items);
	});
var author$project$Configuration$OperationOnDrop$RotateOut$config = {cB: author$project$Configuration$OperationOnDrop$RotateOut$updateColors, eI: 0, c8: 3, fr: 1};
var author$project$Configuration$OperationOnDrop$RotateOut$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$RotateOut$config, author$project$Configuration$OperationOnDrop$RotateOut$MyMsg);
var author$project$Configuration$OperationOnDrop$RotateOut$initialModel = {z: author$project$Configuration$OperationOnDrop$RotateOut$system.z, A: author$project$Configuration$OperationOnDrop$RotateOut$data};
var author$project$Configuration$OperationOnDrop$Swap$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$Swap$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$Swap$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$Swap$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$Swap$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$Swap$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$Swap$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$Swap$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$Swap$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$Swap$Item, value, author$project$Configuration$OperationOnDrop$Swap$sourceColor) : (_Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$Swap$Item, value, author$project$Configuration$OperationOnDrop$Swap$targetColor) : A2(author$project$Configuration$OperationOnDrop$Swap$Item, value, author$project$Configuration$OperationOnDrop$Swap$baseColor));
				}),
			items) : items;
	});
var author$project$Configuration$OperationOnDrop$Swap$config = {cB: author$project$Configuration$OperationOnDrop$Swap$updateColors, eI: 0, c8: 4, fr: 1};
var author$project$Configuration$OperationOnDrop$Swap$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$Swap$config, author$project$Configuration$OperationOnDrop$Swap$MyMsg);
var author$project$Configuration$OperationOnDrop$Swap$initialModel = {z: author$project$Configuration$OperationOnDrop$Swap$system.z, A: author$project$Configuration$OperationOnDrop$Swap$data};
var author$project$Configuration$OperationOnDrop$Unmove$Item = F2(
	function (value, color) {
		return {b3: color, co: value};
	});
var author$project$Configuration$OperationOnDrop$Unmove$baseColor = 'dimgray';
var author$project$Configuration$OperationOnDrop$Unmove$data = A2(
	elm$core$List$map,
	function (i) {
		return A2(
			author$project$Configuration$OperationOnDrop$Unmove$Item,
			elm$core$String$fromInt(i),
			author$project$Configuration$OperationOnDrop$Unmove$baseColor);
	},
	A2(elm$core$List$range, 0, 9));
var author$project$Configuration$OperationOnDrop$Unmove$MyMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$Unmove$sourceColor = 'green';
var author$project$Configuration$OperationOnDrop$Unmove$targetColor = 'red';
var author$project$Configuration$OperationOnDrop$Unmove$updateColors = F3(
	function (dragIndex, dropIndex, items) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
			elm$core$List$indexedMap,
			F2(
				function (i, _n0) {
					var value = _n0.co;
					return _Utils_eq(i, dragIndex) ? A2(author$project$Configuration$OperationOnDrop$Unmove$Item, value, author$project$Configuration$OperationOnDrop$Unmove$targetColor) : (_Utils_eq(i, dropIndex) ? A2(author$project$Configuration$OperationOnDrop$Unmove$Item, value, author$project$Configuration$OperationOnDrop$Unmove$sourceColor) : A2(author$project$Configuration$OperationOnDrop$Unmove$Item, value, author$project$Configuration$OperationOnDrop$Unmove$baseColor));
				}),
			items) : items;
	});
var author$project$Configuration$OperationOnDrop$Unmove$config = {cB: author$project$Configuration$OperationOnDrop$Unmove$updateColors, eI: 0, c8: 5, fr: 1};
var author$project$Configuration$OperationOnDrop$Unmove$system = A2(author$project$DnDList$create, author$project$Configuration$OperationOnDrop$Unmove$config, author$project$Configuration$OperationOnDrop$Unmove$MyMsg);
var author$project$Configuration$OperationOnDrop$Unmove$initialModel = {z: author$project$Configuration$OperationOnDrop$Unmove$system.z, A: author$project$Configuration$OperationOnDrop$Unmove$data};
var author$project$Configuration$OperationOnDrop$Root$initialModel = {
	aP: _List_fromArray(
		[
			author$project$Configuration$OperationOnDrop$Root$InsertAfter(author$project$Configuration$OperationOnDrop$InsertAfter$initialModel),
			author$project$Configuration$OperationOnDrop$Root$InsertBefore(author$project$Configuration$OperationOnDrop$InsertBefore$initialModel),
			author$project$Configuration$OperationOnDrop$Root$RotateIn(author$project$Configuration$OperationOnDrop$RotateIn$initialModel),
			author$project$Configuration$OperationOnDrop$Root$RotateOut(author$project$Configuration$OperationOnDrop$RotateOut$initialModel),
			author$project$Configuration$OperationOnDrop$Root$Swap(author$project$Configuration$OperationOnDrop$Swap$initialModel),
			author$project$Configuration$OperationOnDrop$Root$Unmove(author$project$Configuration$OperationOnDrop$Unmove$initialModel)
		]),
	bI: 0
};
var author$project$Configuration$Root$Groups = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$Root$Movement = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Root$OperationOnDrag = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Root$OperationOnDrop = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Root$selectExample = function (slug) {
	switch (slug) {
		case 'operations-drag':
			return author$project$Configuration$Root$OperationOnDrag(author$project$Configuration$OperationOnDrag$Root$initialModel);
		case 'operations-drop':
			return author$project$Configuration$Root$OperationOnDrop(author$project$Configuration$OperationOnDrop$Root$initialModel);
		case 'movement':
			return author$project$Configuration$Root$Movement(author$project$Configuration$Movement$Root$initialModel);
		case 'groups':
			return author$project$Configuration$Root$Groups(author$project$Configuration$Groups$Root$initialModel);
		default:
			return author$project$Configuration$Root$OperationOnDrag(author$project$Configuration$OperationOnDrag$Root$initialModel);
	}
};
var author$project$Configuration$Root$init = function (slug) {
	return _Utils_Tuple2(
		{
			r: author$project$Configuration$Root$selectExample(slug),
			ae: slug
		},
		elm$core$Platform$Cmd$none);
};
var author$project$Gallery$Puzzle$NewGame = function (a) {
	return {$: 0, a: a};
};
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$Gallery$Puzzle$get = F2(
	function (index, list) {
		return elm$core$List$head(
			A2(elm$core$List$drop, index, list));
	});
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var elm$random$Random$Generator = elm$core$Basics$identity;
var elm$random$Random$constant = function (value) {
	return function (seed) {
		return _Utils_Tuple2(value, seed);
	};
};
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$random$Random$next = function (_n0) {
	var state0 = _n0.a;
	var incr = _n0.b;
	return A2(elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var elm$core$Bitwise$xor = _Bitwise_xor;
var elm$random$Random$peel = function (_n0) {
	var state = _n0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var elm$random$Random$int = F2(
	function (a, b) {
		return function (seed0) {
			var _n0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
			var lo = _n0.a;
			var hi = _n0.b;
			var range = (hi - lo) + 1;
			if (!((range - 1) & range)) {
				return _Utils_Tuple2(
					(((range - 1) & elm$random$Random$peel(seed0)) >>> 0) + lo,
					elm$random$Random$next(seed0));
			} else {
				var threshhold = (((-range) >>> 0) % range) >>> 0;
				var accountForBias = function (seed) {
					accountForBias:
					while (true) {
						var x = elm$random$Random$peel(seed);
						var seedN = elm$random$Random$next(seed);
						if (_Utils_cmp(x, threshhold) < 0) {
							var $temp$seed = seedN;
							seed = $temp$seed;
							continue accountForBias;
						} else {
							return _Utils_Tuple2((x % range) + lo, seedN);
						}
					}
				};
				return accountForBias(seed0);
			}
		};
	});
var elm$random$Random$map = F2(
	function (func, _n0) {
		var genA = _n0;
		return function (seed0) {
			var _n1 = genA(seed0);
			var a = _n1.a;
			var seed1 = _n1.b;
			return _Utils_Tuple2(
				func(a),
				seed1);
		};
	});
var author$project$Gallery$Puzzle$choose = function (list) {
	if (elm$core$List$isEmpty(list)) {
		return elm$random$Random$constant(
			_Utils_Tuple2(elm$core$Maybe$Nothing, list));
	} else {
		var lastIndex = elm$core$List$length(list) - 1;
		var gen = A2(elm$random$Random$int, 0, lastIndex);
		var front = function (i) {
			return A2(elm$core$List$take, i, list);
		};
		var back = function (i) {
			return A2(elm$core$List$drop, i + 1, list);
		};
		return A2(
			elm$random$Random$map,
			function (index) {
				return _Utils_Tuple2(
					A2(author$project$Gallery$Puzzle$get, index, list),
					A2(
						elm$core$List$append,
						front(index),
						back(index)));
			},
			gen);
	}
};
var elm$random$Random$andThen = F2(
	function (callback, _n0) {
		var genA = _n0;
		return function (seed) {
			var _n1 = genA(seed);
			var result = _n1.a;
			var newSeed = _n1.b;
			var _n2 = callback(result);
			var genB = _n2;
			return genB(newSeed);
		};
	});
var author$project$Gallery$Puzzle$shuffle = function (list) {
	if (elm$core$List$isEmpty(list)) {
		return elm$random$Random$constant(list);
	} else {
		var helper = function (_n0) {
			var done = _n0.a;
			var remaining = _n0.b;
			return A2(
				elm$random$Random$andThen,
				function (_n1) {
					var m_val = _n1.a;
					var shorter = _n1.b;
					if (m_val.$ === 1) {
						return elm$random$Random$constant(
							_Utils_Tuple2(done, shorter));
					} else {
						var val = m_val.a;
						return helper(
							_Utils_Tuple2(
								A2(elm$core$List$cons, val, done),
								shorter));
					}
				},
				author$project$Gallery$Puzzle$choose(remaining));
		};
		return A2(
			elm$random$Random$map,
			elm$core$Tuple$first,
			helper(
				_Utils_Tuple2(_List_Nil, list)));
	}
};
var author$project$Gallery$Puzzle$Item = F3(
	function (group, value, color) {
		return {b3: color, t: group, co: value};
	});
var author$project$Gallery$Puzzle$blue = '#448ff8';
var author$project$Gallery$Puzzle$cyan = '#44bcf8';
var author$project$Gallery$Puzzle$indigo = '#4462f8';
var author$project$Gallery$Puzzle$purple = '#5344f8';
var author$project$Gallery$Puzzle$solution = _List_fromArray(
	[
		A3(author$project$Gallery$Puzzle$Item, 0, 'A', author$project$Gallery$Puzzle$cyan),
		A3(author$project$Gallery$Puzzle$Item, 0, 'B', author$project$Gallery$Puzzle$cyan),
		A3(author$project$Gallery$Puzzle$Item, 0, 'C', author$project$Gallery$Puzzle$cyan),
		A3(author$project$Gallery$Puzzle$Item, 0, 'D', author$project$Gallery$Puzzle$cyan),
		A3(author$project$Gallery$Puzzle$Item, 1, 'Ա', author$project$Gallery$Puzzle$blue),
		A3(author$project$Gallery$Puzzle$Item, 1, 'Բ', author$project$Gallery$Puzzle$blue),
		A3(author$project$Gallery$Puzzle$Item, 1, 'Գ', author$project$Gallery$Puzzle$blue),
		A3(author$project$Gallery$Puzzle$Item, 1, 'Դ', author$project$Gallery$Puzzle$blue),
		A3(author$project$Gallery$Puzzle$Item, 2, '1', author$project$Gallery$Puzzle$indigo),
		A3(author$project$Gallery$Puzzle$Item, 2, '2', author$project$Gallery$Puzzle$indigo),
		A3(author$project$Gallery$Puzzle$Item, 2, '3', author$project$Gallery$Puzzle$indigo),
		A3(author$project$Gallery$Puzzle$Item, 2, '4', author$project$Gallery$Puzzle$indigo),
		A3(author$project$Gallery$Puzzle$Item, 3, 'α', author$project$Gallery$Puzzle$purple),
		A3(author$project$Gallery$Puzzle$Item, 3, 'β', author$project$Gallery$Puzzle$purple),
		A3(author$project$Gallery$Puzzle$Item, 3, 'γ', author$project$Gallery$Puzzle$purple),
		A3(author$project$Gallery$Puzzle$Item, 3, 'δ', author$project$Gallery$Puzzle$purple)
	]);
var elm$random$Random$Generate = elm$core$Basics$identity;
var elm$random$Random$initialSeed = function (x) {
	var _n0 = elm$random$Random$next(
		A2(elm$random$Random$Seed, 0, 1013904223));
	var state1 = _n0.a;
	var incr = _n0.b;
	var state2 = (state1 + x) >>> 0;
	return elm$random$Random$next(
		A2(elm$random$Random$Seed, state2, incr));
};
var elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$time$Time$customZone = elm$time$Time$Zone;
var elm$time$Time$Posix = elm$core$Basics$identity;
var elm$time$Time$millisToPosix = elm$core$Basics$identity;
var elm$time$Time$now = _Time_now(elm$time$Time$millisToPosix);
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0;
	return millis;
};
var elm$random$Random$init = A2(
	elm$core$Task$andThen,
	function (time) {
		return elm$core$Task$succeed(
			elm$random$Random$initialSeed(
				elm$time$Time$posixToMillis(time)));
	},
	elm$time$Time$now);
var elm$random$Random$step = F2(
	function (_n0, seed) {
		var generator = _n0;
		return generator(seed);
	});
var elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a;
			var rest = commands.b;
			var _n1 = A2(elm$random$Random$step, generator, seed);
			var value = _n1.a;
			var newSeed = _n1.b;
			return A2(
				elm$core$Task$andThen,
				function (_n2) {
					return A3(elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2(elm$core$Platform$sendToApp, router, value));
		}
	});
var elm$random$Random$onSelfMsg = F3(
	function (_n0, _n1, seed) {
		return elm$core$Task$succeed(seed);
	});
var elm$random$Random$cmdMap = F2(
	function (func, _n0) {
		var generator = _n0;
		return A2(elm$random$Random$map, func, generator);
	});
_Platform_effectManagers['Random'] = _Platform_createManager(elm$random$Random$init, elm$random$Random$onEffects, elm$random$Random$onSelfMsg, elm$random$Random$cmdMap);
var elm$random$Random$command = _Platform_leaf('Random');
var elm$random$Random$generate = F2(
	function (tagger, generator) {
		return elm$random$Random$command(
			A2(elm$random$Random$map, tagger, generator));
	});
var author$project$Gallery$Puzzle$commands = A2(
	elm$random$Random$generate,
	author$project$Gallery$Puzzle$NewGame,
	author$project$Gallery$Puzzle$shuffle(author$project$Gallery$Puzzle$solution));
var author$project$Gallery$Root$PuzzleMsg = function (a) {
	return {$: 1, a: a};
};
var elm$core$Platform$Cmd$map = _Platform_map;
var author$project$Gallery$Root$commands = elm$core$Platform$Cmd$batch(
	_List_fromArray(
		[
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$PuzzleMsg, author$project$Gallery$Puzzle$commands)
		]));
var author$project$Gallery$Hanoi$Disk = F4(
	function (tower, width, startColor, solvedColor) {
		return {ck: solvedColor, bV: startColor, aC: tower, cp: width};
	});
var author$project$Gallery$Hanoi$data = _List_fromArray(
	[
		A4(author$project$Gallery$Hanoi$Disk, 0, 300, 'transparent', 'transparent'),
		A4(author$project$Gallery$Hanoi$Disk, 0, 60, '#aa7d1e', '#fbe300'),
		A4(author$project$Gallery$Hanoi$Disk, 0, 120, '#aa711e', '#fbce00'),
		A4(author$project$Gallery$Hanoi$Disk, 0, 180, '#aa651e', '#fbb900'),
		A4(author$project$Gallery$Hanoi$Disk, 0, 240, '#aa5a1e', '#fba400'),
		A4(author$project$Gallery$Hanoi$Disk, 1, 300, 'transparent', 'transparent'),
		A4(author$project$Gallery$Hanoi$Disk, 2, 300, 'transparent', 'transparent')
	]);
var author$project$Gallery$Hanoi$MyMsg = elm$core$Basics$identity;
var author$project$Gallery$Hanoi$updateTower = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragDisk, dropDisk) {
									return _Utils_update(
										dragDisk,
										{aC: dropDisk.aC});
								}),
							_List_fromArray(
								[item]),
							drop) : _List_fromArray(
							[item]);
					}),
				list));
	});
var author$project$Gallery$Hanoi$config = {cB: author$project$Gallery$Hanoi$updateTower, eI: 0, c8: 0, fr: 1};
var author$project$Gallery$Hanoi$system = A2(author$project$DnDList$create, author$project$Gallery$Hanoi$config, elm$core$Basics$identity);
var author$project$Gallery$Hanoi$initialModel = {U: author$project$Gallery$Hanoi$data, z: author$project$Gallery$Hanoi$system.z, bm: false};
var author$project$Gallery$Puzzle$MyMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$DnDList$Groups$OnDrop = 1;
var author$project$Gallery$Puzzle$compareByGroup = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.t, dropItem.t);
	});
var author$project$Gallery$Puzzle$updateOnGroupChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		var drag = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dragIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{t: dropItem.t});
								}),
							_List_fromArray(
								[item]),
							drop) : (_Utils_eq(index, dropIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dropItem,
										{t: dragItem.t});
								}),
							drag,
							_List_fromArray(
								[item])) : _List_fromArray(
							[item]));
					}),
				list));
	});
var author$project$Gallery$Puzzle$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Gallery$Puzzle$updateOnGroupChange, eb: author$project$Gallery$Puzzle$compareByGroup, c8: 4},
	eI: 0,
	c8: 4,
	fr: 1
};
var author$project$Gallery$Puzzle$system = A2(author$project$DnDList$Groups$create, author$project$Gallery$Puzzle$config, author$project$Gallery$Puzzle$MyMsg);
var author$project$Gallery$Puzzle$initialModel = {z: author$project$Gallery$Puzzle$system.z, A: _List_Nil};
var author$project$Gallery$Root$Hanoi = function (a) {
	return {$: 0, a: a};
};
var author$project$Gallery$Root$Puzzle = function (a) {
	return {$: 1, a: a};
};
var author$project$Gallery$Root$Shapes = function (a) {
	return {$: 2, a: a};
};
var author$project$Gallery$Root$TaskBoard = function (a) {
	return {$: 4, a: a};
};
var author$project$Gallery$Root$TryOn = function (a) {
	return {$: 3, a: a};
};
var author$project$Gallery$Shapes$Circle = 0;
var author$project$Gallery$Shapes$Cross = 1;
var author$project$Gallery$Shapes$Item = F4(
	function (shape, color, attempts, solved) {
		return {aF: attempts, b3: color, a1: shape, bm: solved};
	});
var author$project$Gallery$Shapes$Square = 2;
var author$project$Gallery$Shapes$Triangle = 3;
var author$project$Gallery$Shapes$data = _List_fromArray(
	[
		A4(author$project$Gallery$Shapes$Item, 0, '#d82775', 0, false),
		A4(author$project$Gallery$Shapes$Item, 1, '#ffcf00', 0, false),
		A4(author$project$Gallery$Shapes$Item, 3, '#00b2d4', 0, false),
		A4(author$project$Gallery$Shapes$Item, 2, '#90e200', 0, false),
		A4(author$project$Gallery$Shapes$Item, 3, 'dimgray', 0, false),
		A4(author$project$Gallery$Shapes$Item, 2, 'dimgray', 0, false),
		A4(author$project$Gallery$Shapes$Item, 1, 'dimgray', 0, false),
		A4(author$project$Gallery$Shapes$Item, 0, 'dimgray', 0, false)
	]);
var author$project$Gallery$Shapes$MyMsg = elm$core$Basics$identity;
var elm$core$Basics$not = _Basics_not;
var author$project$Gallery$Shapes$updateShapes = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		var drag = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dragIndex, list));
		var fit = A3(
			elm$core$List$foldl,
			elm$core$Basics$or,
			false,
			A3(
				elm$core$List$map2,
				F2(
					function (dragItem, dropItem) {
						return _Utils_eq(dragItem.a1, dropItem.a1);
					}),
				drag,
				drop));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return (_Utils_eq(index, dragIndex) && fit) ? _List_fromArray(
							[
								_Utils_update(
								item,
								{b3: 'transparent', bm: true})
							]) : ((_Utils_eq(index, dropIndex) && fit) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dropItem,
										{aF: dropItem.aF + 1, b3: dragItem.b3, bm: true});
								}),
							drag,
							_List_fromArray(
								[item])) : ((_Utils_eq(index, dropIndex) && (!fit)) ? _List_fromArray(
							[
								_Utils_update(
								item,
								{aF: item.aF + 1})
							]) : _List_fromArray(
							[item])));
					}),
				list));
	});
var author$project$Gallery$Shapes$config = {cB: author$project$Gallery$Shapes$updateShapes, eI: 0, c8: 5, fr: 1};
var author$project$Gallery$Shapes$system = A2(author$project$DnDList$create, author$project$Gallery$Shapes$config, elm$core$Basics$identity);
var author$project$Gallery$Shapes$initialModel = {z: author$project$Gallery$Shapes$system.z, A: author$project$Gallery$Shapes$data};
var author$project$Gallery$TaskBoard$CardMoved = function (a) {
	return {$: 0, a: a};
};
var author$project$DnDList$Groups$RotateOut = 3;
var author$project$Gallery$TaskBoard$compareByActivity = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.v, dropItem.v);
	});
var author$project$Gallery$TaskBoard$updateOnActivityChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{v: dropItem.v});
								}),
							_List_fromArray(
								[item]),
							drop) : _List_fromArray(
							[item]);
					}),
				list));
	});
var author$project$Gallery$TaskBoard$cardConfig = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Gallery$TaskBoard$updateOnActivityChange, eb: author$project$Gallery$TaskBoard$compareByActivity, c8: 1},
	eI: 0,
	c8: 3,
	fr: 0
};
var author$project$Gallery$TaskBoard$cardSystem = A2(author$project$DnDList$Groups$create, author$project$Gallery$TaskBoard$cardConfig, author$project$Gallery$TaskBoard$CardMoved);
var author$project$Gallery$TaskBoard$ColumnMoved = function (a) {
	return {$: 1, a: a};
};
var author$project$Gallery$TaskBoard$columnConfig = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 3,
	fr: 0
};
var author$project$Gallery$TaskBoard$columnSystem = A2(author$project$DnDList$create, author$project$Gallery$TaskBoard$columnConfig, author$project$Gallery$TaskBoard$ColumnMoved);
var author$project$Gallery$TaskBoard$Card = F2(
	function (activity, description) {
		return {v: activity, aN: description};
	});
var author$project$Gallery$TaskBoard$Doing = 1;
var author$project$Gallery$TaskBoard$Done = 2;
var author$project$Gallery$TaskBoard$ToDo = 0;
var author$project$Gallery$TaskBoard$gatheredByActivity = _List_fromArray(
	[
		A2(author$project$Gallery$TaskBoard$Card, 0, 'D'),
		A2(author$project$Gallery$TaskBoard$Card, 0, 'B'),
		A2(author$project$Gallery$TaskBoard$Card, 0, 'A'),
		A2(author$project$Gallery$TaskBoard$Card, 0, ''),
		A2(author$project$Gallery$TaskBoard$Card, 1, 'C'),
		A2(author$project$Gallery$TaskBoard$Card, 1, 'F'),
		A2(author$project$Gallery$TaskBoard$Card, 1, ''),
		A2(author$project$Gallery$TaskBoard$Card, 2, 'G'),
		A2(author$project$Gallery$TaskBoard$Card, 2, 'E'),
		A2(author$project$Gallery$TaskBoard$Card, 2, '')
	]);
var author$project$Gallery$TaskBoard$initialModel = {G: author$project$Gallery$TaskBoard$cardSystem.z, H: author$project$Gallery$TaskBoard$gatheredByActivity, I: author$project$Gallery$TaskBoard$columnSystem.z};
var author$project$Gallery$TryOn$Color = 1;
var author$project$Gallery$TryOn$Item = F4(
	function (property, width, height, color) {
		return {b3: color, b8: height, bk: property, cp: width};
	});
var author$project$Gallery$TryOn$Size = 0;
var author$project$Gallery$TryOn$gray = 'dimgray';
var author$project$Gallery$TryOn$lavender = '#956dbd';
var author$project$Gallery$TryOn$papayaWhip = '#ffead3';
var author$project$Gallery$TryOn$pink = '#fec8d8';
var author$project$Gallery$TryOn$violet = '#d291bc';
var author$project$Gallery$TryOn$data = _List_fromArray(
	[
		A4(author$project$Gallery$TryOn$Item, 1, 1, 1, author$project$Gallery$TryOn$papayaWhip),
		A4(author$project$Gallery$TryOn$Item, 1, 1, 1, author$project$Gallery$TryOn$pink),
		A4(author$project$Gallery$TryOn$Item, 1, 1, 1, author$project$Gallery$TryOn$violet),
		A4(author$project$Gallery$TryOn$Item, 1, 1, 1, author$project$Gallery$TryOn$lavender),
		A4(author$project$Gallery$TryOn$Item, 0, 1, 2, author$project$Gallery$TryOn$gray),
		A4(author$project$Gallery$TryOn$Item, 0, 2, 2, author$project$Gallery$TryOn$gray),
		A4(author$project$Gallery$TryOn$Item, 0, 1, 3, author$project$Gallery$TryOn$gray),
		A4(author$project$Gallery$TryOn$Item, 0, 2, 3, author$project$Gallery$TryOn$gray),
		A4(author$project$Gallery$TryOn$Item, 0, 3, 3, author$project$Gallery$TryOn$gray)
	]);
var author$project$Gallery$TryOn$MyMsg = elm$core$Basics$identity;
var author$project$Gallery$TryOn$updateColor = F3(
	function (dragIndex, dropIndex, list) {
		var drag = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dragIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						if (_Utils_eq(index, dropIndex)) {
							return A3(
								elm$core$List$map2,
								F2(
									function (dragItem, dropItem) {
										return _Utils_update(
											dropItem,
											{b3: dragItem.b3});
									}),
								drag,
								_List_fromArray(
									[item]));
						} else {
							if (_Utils_eq(index, dragIndex)) {
								var _n0 = item.bk;
								if (!_n0) {
									return _List_fromArray(
										[
											_Utils_update(
											item,
											{b3: author$project$Gallery$TryOn$gray})
										]);
								} else {
									return _List_fromArray(
										[item]);
								}
							} else {
								return _List_fromArray(
									[item]);
							}
						}
					}),
				list));
	});
var author$project$Gallery$TryOn$config = {cB: author$project$Gallery$TryOn$updateColor, eI: 0, c8: 5, fr: 1};
var author$project$Gallery$TryOn$system = A2(author$project$DnDList$create, author$project$Gallery$TryOn$config, elm$core$Basics$identity);
var author$project$Gallery$TryOn$initialModel = {z: author$project$Gallery$TryOn$system.z, A: author$project$Gallery$TryOn$data};
var author$project$Gallery$Root$selectExample = function (slug) {
	switch (slug) {
		case 'hanoi':
			return author$project$Gallery$Root$Hanoi(author$project$Gallery$Hanoi$initialModel);
		case 'puzzle':
			return author$project$Gallery$Root$Puzzle(author$project$Gallery$Puzzle$initialModel);
		case 'shapes':
			return author$project$Gallery$Root$Shapes(author$project$Gallery$Shapes$initialModel);
		case 'try-on':
			return author$project$Gallery$Root$TryOn(author$project$Gallery$TryOn$initialModel);
		case 'taskboard':
			return author$project$Gallery$Root$TaskBoard(author$project$Gallery$TaskBoard$initialModel);
		default:
			return author$project$Gallery$Root$Hanoi(author$project$Gallery$Hanoi$initialModel);
	}
};
var author$project$Gallery$Root$init = function (slug) {
	return _Utils_Tuple2(
		{
			r: author$project$Gallery$Root$selectExample(slug),
			ae: slug
		},
		author$project$Gallery$Root$commands);
};
var author$project$Introduction$Masonry$NewMasonry = function (a) {
	return {$: 0, a: a};
};
var author$project$Introduction$Masonry$colors = _List_fromArray(
	['#acfe2f', '#cefe2f', '#f0fe2f', '#feea2f', '#fec72f', '#fea52f', '#fe832f', '#fe612f', '#fe3f2f']);
var elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _n0 = gen(seed);
				var value = _n0.a;
				var newSeed = _n0.b;
				var $temp$revList = A2(elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var elm$random$Random$list = F2(
	function (n, _n0) {
		var gen = _n0;
		return function (seed) {
			return A4(elm$random$Random$listHelp, _List_Nil, n, gen, seed);
		};
	});
var author$project$Introduction$Masonry$commands = A2(
	elm$random$Random$generate,
	author$project$Introduction$Masonry$NewMasonry,
	A2(
		elm$random$Random$list,
		elm$core$List$length(author$project$Introduction$Masonry$colors),
		A2(elm$random$Random$int, 50, 200)));
var author$project$Introduction$Root$MasonryMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Introduction$Root$commands = elm$core$Platform$Cmd$batch(
	_List_fromArray(
		[
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$MasonryMsg, author$project$Introduction$Masonry$commands)
		]));
var author$project$Introduction$Basic$data = _List_fromArray(
	['Apples', 'Bananas', 'Cherries', 'Dates']);
var author$project$Introduction$Basic$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Basic$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 3,
	fr: 0
};
var author$project$Introduction$Basic$system = A2(author$project$DnDList$create, author$project$Introduction$Basic$config, elm$core$Basics$identity);
var author$project$Introduction$Basic$initialModel = {z: author$project$Introduction$Basic$system.z, A: author$project$Introduction$Basic$data};
var author$project$Introduction$BasicElmUI$data = _List_fromArray(
	['Apples', 'Bananas', 'Cherries', 'Dates']);
var author$project$Introduction$BasicElmUI$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$BasicElmUI$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 3,
	fr: 0
};
var author$project$Introduction$BasicElmUI$system = A2(author$project$DnDList$create, author$project$Introduction$BasicElmUI$config, elm$core$Basics$identity);
var author$project$Introduction$BasicElmUI$initialModel = {z: author$project$Introduction$BasicElmUI$system.z, A: author$project$Introduction$BasicElmUI$data};
var author$project$Introduction$Groups$Bottom = 1;
var author$project$Introduction$Groups$Item = F3(
	function (group, value, color) {
		return {b3: color, t: group, co: value};
	});
var author$project$Introduction$Groups$Top = 0;
var author$project$Introduction$Groups$blue = '#0067c3';
var author$project$Introduction$Groups$red = '#c30005';
var author$project$Introduction$Groups$transparent = 'transparent';
var author$project$Introduction$Groups$gatheredByGroup = _List_fromArray(
	[
		A3(author$project$Introduction$Groups$Item, 0, 'C', author$project$Introduction$Groups$blue),
		A3(author$project$Introduction$Groups$Item, 0, '2', author$project$Introduction$Groups$red),
		A3(author$project$Introduction$Groups$Item, 0, 'A', author$project$Introduction$Groups$blue),
		A3(author$project$Introduction$Groups$Item, 0, '', author$project$Introduction$Groups$transparent),
		A3(author$project$Introduction$Groups$Item, 1, '3', author$project$Introduction$Groups$red),
		A3(author$project$Introduction$Groups$Item, 1, 'B', author$project$Introduction$Groups$blue),
		A3(author$project$Introduction$Groups$Item, 1, '1', author$project$Introduction$Groups$red),
		A3(author$project$Introduction$Groups$Item, 1, '', author$project$Introduction$Groups$transparent)
	]);
var author$project$Introduction$Groups$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Groups$compareByGroup = F2(
	function (dragItem, dropItem) {
		return _Utils_eq(dragItem.t, dropItem.t);
	});
var author$project$Introduction$Groups$updateOnGroupChange = F3(
	function (dragIndex, dropIndex, list) {
		var drop = A2(
			elm$core$List$take,
			1,
			A2(elm$core$List$drop, dropIndex, list));
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, dragIndex) ? A3(
							elm$core$List$map2,
							F2(
								function (dragItem, dropItem) {
									return _Utils_update(
										dragItem,
										{t: dropItem.t});
								}),
							_List_fromArray(
								[item]),
							drop) : _List_fromArray(
							[item]);
					}),
				list));
	});
var author$project$Introduction$Groups$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	es: {cB: author$project$Introduction$Groups$updateOnGroupChange, eb: author$project$Introduction$Groups$compareByGroup, c8: 1},
	eI: 0,
	c8: 3,
	fr: 0
};
var author$project$Introduction$Groups$system = A2(author$project$DnDList$Groups$create, author$project$Introduction$Groups$config, elm$core$Basics$identity);
var author$project$Introduction$Groups$initialModel = {z: author$project$Introduction$Groups$system.z, A: author$project$Introduction$Groups$gatheredByGroup};
var author$project$Introduction$Handle$data = _List_fromArray(
	['Apples', 'Bananas', 'Cherries', 'Dates']);
var author$project$Introduction$Handle$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Handle$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Handle$system = A2(author$project$DnDList$create, author$project$Introduction$Handle$config, elm$core$Basics$identity);
var author$project$Introduction$Handle$initialModel = {z: author$project$Introduction$Handle$system.z, aT: author$project$Introduction$Handle$data};
var author$project$Introduction$Independents$blueData = _List_fromArray(
	['A', 'B', 'C', 'D']);
var author$project$Introduction$Independents$BlueMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Introduction$Independents$blueConfig = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Independents$blueSystem = A2(author$project$DnDList$create, author$project$Introduction$Independents$blueConfig, author$project$Introduction$Independents$BlueMsg);
var author$project$Introduction$Independents$redData = _List_fromArray(
	['1', '2', '3', '4']);
var author$project$Introduction$Independents$RedMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Introduction$Independents$redConfig = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Independents$redSystem = A2(author$project$DnDList$create, author$project$Introduction$Independents$redConfig, author$project$Introduction$Independents$RedMsg);
var author$project$Introduction$Independents$initialModel = {R: author$project$Introduction$Independents$blueSystem.z, aI: author$project$Introduction$Independents$blueData, ad: author$project$Introduction$Independents$redSystem.z, a0: author$project$Introduction$Independents$redData};
var author$project$Introduction$Keyed$data = A2(
	elm$core$List$map,
	function (v) {
		return _Utils_Tuple2('key-' + v, v);
	},
	_List_fromArray(
		['A', 'B', 'C', 'D']));
var author$project$Introduction$Keyed$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Keyed$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Keyed$system = A2(author$project$DnDList$create, author$project$Introduction$Keyed$config, elm$core$Basics$identity);
var author$project$Introduction$Keyed$initialModel = {z: author$project$Introduction$Keyed$system.z, A: author$project$Introduction$Keyed$data};
var author$project$Introduction$Margins$data = _List_fromArray(
	['A', 'B', 'C', 'D']);
var author$project$Introduction$Margins$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Margins$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Margins$system = A2(author$project$DnDList$create, author$project$Introduction$Margins$config, elm$core$Basics$identity);
var author$project$Introduction$Margins$initialModel = {z: author$project$Introduction$Margins$system.z, A: author$project$Introduction$Margins$data};
var author$project$Introduction$Masonry$MyMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Introduction$Masonry$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Masonry$system = A2(author$project$DnDList$create, author$project$Introduction$Masonry$config, author$project$Introduction$Masonry$MyMsg);
var author$project$Introduction$Masonry$initialModel = {z: author$project$Introduction$Masonry$system.z, A: _List_Nil};
var author$project$Introduction$Resize$blue = '#2592d3';
var author$project$Introduction$Resize$green = '#25D366';
var author$project$Introduction$Resize$orange = '#dc9a39';
var author$project$Introduction$Resize$red = '#dc4839';
var author$project$Introduction$Resize$yellow = '#cddc39';
var author$project$Introduction$Resize$data = _List_fromArray(
	[author$project$Introduction$Resize$green, author$project$Introduction$Resize$yellow, author$project$Introduction$Resize$blue, author$project$Introduction$Resize$orange, author$project$Introduction$Resize$red]);
var author$project$Introduction$Resize$MyMsg = elm$core$Basics$identity;
var author$project$Introduction$Resize$config = {
	cB: F3(
		function (_n0, _n1, list) {
			return list;
		}),
	eI: 0,
	c8: 4,
	fr: 0
};
var author$project$Introduction$Resize$system = A2(author$project$DnDList$create, author$project$Introduction$Resize$config, elm$core$Basics$identity);
var author$project$Introduction$Resize$initialModel = {aM: author$project$Introduction$Resize$data, z: author$project$Introduction$Resize$system.z};
var author$project$Introduction$Root$Basic = function (a) {
	return {$: 0, a: a};
};
var author$project$Introduction$Root$BasicElmUI = function (a) {
	return {$: 1, a: a};
};
var author$project$Introduction$Root$Groups = function (a) {
	return {$: 8, a: a};
};
var author$project$Introduction$Root$Handle = function (a) {
	return {$: 2, a: a};
};
var author$project$Introduction$Root$Independents = function (a) {
	return {$: 7, a: a};
};
var author$project$Introduction$Root$Keyed = function (a) {
	return {$: 3, a: a};
};
var author$project$Introduction$Root$Margins = function (a) {
	return {$: 4, a: a};
};
var author$project$Introduction$Root$Masonry = function (a) {
	return {$: 5, a: a};
};
var author$project$Introduction$Root$Resize = function (a) {
	return {$: 6, a: a};
};
var author$project$Introduction$Root$selectExample = function (slug) {
	switch (slug) {
		case 'basic':
			return author$project$Introduction$Root$Basic(author$project$Introduction$Basic$initialModel);
		case 'basic-elm-ui':
			return author$project$Introduction$Root$BasicElmUI(author$project$Introduction$BasicElmUI$initialModel);
		case 'handle':
			return author$project$Introduction$Root$Handle(author$project$Introduction$Handle$initialModel);
		case 'keyed':
			return author$project$Introduction$Root$Keyed(author$project$Introduction$Keyed$initialModel);
		case 'margins':
			return author$project$Introduction$Root$Margins(author$project$Introduction$Margins$initialModel);
		case 'masonry':
			return author$project$Introduction$Root$Masonry(author$project$Introduction$Masonry$initialModel);
		case 'resize':
			return author$project$Introduction$Root$Resize(author$project$Introduction$Resize$initialModel);
		case 'independents':
			return author$project$Introduction$Root$Independents(author$project$Introduction$Independents$initialModel);
		case 'groups':
			return author$project$Introduction$Root$Groups(author$project$Introduction$Groups$initialModel);
		default:
			return author$project$Introduction$Root$Basic(author$project$Introduction$Basic$initialModel);
	}
};
var author$project$Introduction$Root$init = function (slug) {
	return _Utils_Tuple2(
		{
			r: author$project$Introduction$Root$selectExample(slug),
			ae: slug
		},
		author$project$Introduction$Root$commands);
};
var elm$url$Url$Parser$Parser = elm$core$Basics$identity;
var elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {ap: frag, aw: params, ag: unvisited, co: value, aE: visited};
	});
var elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return function (_n0) {
			var visited = _n0.aE;
			var unvisited = _n0.ag;
			var params = _n0.aw;
			var frag = _n0.ap;
			var value = _n0.co;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				var _n2 = stringToSomething(next);
				if (!_n2.$) {
					var nextValue = _n2.a;
					return _List_fromArray(
						[
							A5(
							elm$url$Url$Parser$State,
							A2(elm$core$List$cons, next, visited),
							rest,
							params,
							frag,
							value(nextValue))
						]);
				} else {
					return _List_Nil;
				}
			}
		};
	});
var author$project$Main$slug_ = A2(elm$url$Url$Parser$custom, 'SLUG', elm$core$Maybe$Just);
var author$project$Main$Configuration = function (a) {
	return {$: 3, a: a};
};
var author$project$Main$ConfigurationMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Main$stepConfiguration = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Main$Configuration(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Main$ConfigurationMsg, cmds));
	});
var author$project$Main$Gallery = function (a) {
	return {$: 4, a: a};
};
var author$project$Main$GalleryMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Main$stepGallery = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Main$Gallery(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Main$GalleryMsg, cmds));
	});
var author$project$Main$Introduction = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$IntroductionMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Main$stepIntroduction = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Main$Introduction(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Main$IntroductionMsg, cmds));
	});
var elm$url$Url$Parser$mapState = F2(
	function (func, _n0) {
		var visited = _n0.aE;
		var unvisited = _n0.ag;
		var params = _n0.aw;
		var frag = _n0.ap;
		var value = _n0.co;
		return A5(
			elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var elm$url$Url$Parser$map = F2(
	function (subValue, _n0) {
		var parseArg = _n0;
		return function (_n1) {
			var visited = _n1.aE;
			var unvisited = _n1.ag;
			var params = _n1.aw;
			var frag = _n1.ap;
			var value = _n1.co;
			return A2(
				elm$core$List$map,
				elm$url$Url$Parser$mapState(value),
				parseArg(
					A5(elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
		};
	});
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$url$Url$Parser$oneOf = function (parsers) {
	return function (state) {
		return A2(
			elm$core$List$concatMap,
			function (_n0) {
				var parser = _n0;
				return parser(state);
			},
			parsers);
	};
};
var elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _n1 = state.ag;
			if (!_n1.b) {
				return elm$core$Maybe$Just(state.co);
			} else {
				if ((_n1.a === '') && (!_n1.b.b)) {
					return elm$core$Maybe$Just(state.co);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				elm$core$List$cons,
				segment,
				elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var elm$url$Url$Parser$preparePath = function (path) {
	var _n0 = A2(elm$core$String$split, '/', path);
	if (_n0.b && (_n0.a === '')) {
		var segments = _n0.b;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _n0;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === -1) {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === -1) {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === -1) {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (!_n0.$) {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 1) {
			return elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return elm$core$Maybe$Just(
				A2(elm$core$List$cons, value, list));
		}
	});
var elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _n0 = A2(elm$core$String$split, '=', segment);
		if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
			var rawKey = _n0.a;
			var _n1 = _n0.b;
			var rawValue = _n1.a;
			var _n2 = elm$url$Url$percentDecode(rawKey);
			if (_n2.$ === 1) {
				return dict;
			} else {
				var key = _n2.a;
				var _n3 = elm$url$Url$percentDecode(rawValue);
				if (_n3.$ === 1) {
					return dict;
				} else {
					var value = _n3.a;
					return A3(
						elm$core$Dict$update,
						key,
						elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 1) {
		return elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			elm$core$List$foldr,
			elm$url$Url$Parser$addParam,
			elm$core$Dict$empty,
			A2(elm$core$String$split, '&', qry));
	}
};
var elm$url$Url$Parser$parse = F2(
	function (_n0, url) {
		var parser = _n0;
		return elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					elm$url$Url$Parser$State,
					_List_Nil,
					elm$url$Url$Parser$preparePath(url.dd),
					elm$url$Url$Parser$prepareQuery(url.dj),
					url.cS,
					elm$core$Basics$identity)));
	});
var elm$url$Url$Parser$s = function (str) {
	return function (_n0) {
		var visited = _n0.aE;
		var unvisited = _n0.ag;
		var params = _n0.aw;
		var frag = _n0.ap;
		var value = _n0.co;
		if (!unvisited.b) {
			return _List_Nil;
		} else {
			var next = unvisited.a;
			var rest = unvisited.b;
			return _Utils_eq(next, str) ? _List_fromArray(
				[
					A5(
					elm$url$Url$Parser$State,
					A2(elm$core$List$cons, next, visited),
					rest,
					params,
					frag,
					value)
				]) : _List_Nil;
		}
	};
};
var elm$url$Url$Parser$slash = F2(
	function (_n0, _n1) {
		var parseBefore = _n0;
		var parseAfter = _n1;
		return function (state) {
			return A2(
				elm$core$List$concatMap,
				parseAfter,
				parseBefore(state));
		};
	});
var elm$url$Url$Parser$top = function (state) {
	return _List_fromArray(
		[state]);
};
var author$project$Main$stepUrl = F2(
	function (url, model) {
		var parser = elm$url$Url$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					elm$url$Url$Parser$map,
					A2(
						author$project$Main$stepIntroduction,
						model,
						author$project$Introduction$Root$init('groups')),
					elm$url$Url$Parser$top),
					A2(
					elm$url$Url$Parser$map,
					A2(
						author$project$Main$stepIntroduction,
						model,
						author$project$Introduction$Root$init('groups')),
					elm$url$Url$Parser$s(author$project$Base$base)),
					A2(
					elm$url$Url$Parser$map,
					function (slug) {
						return A2(
							author$project$Main$stepIntroduction,
							model,
							author$project$Introduction$Root$init(slug));
					},
					A2(
						elm$url$Url$Parser$slash,
						elm$url$Url$Parser$s(author$project$Base$base),
						A2(
							elm$url$Url$Parser$slash,
							elm$url$Url$Parser$s('introduction'),
							author$project$Main$slug_))),
					A2(
					elm$url$Url$Parser$map,
					function (slug) {
						return A2(
							author$project$Main$stepConfiguration,
							model,
							author$project$Configuration$Root$init(slug));
					},
					A2(
						elm$url$Url$Parser$slash,
						elm$url$Url$Parser$s(author$project$Base$base),
						A2(
							elm$url$Url$Parser$slash,
							elm$url$Url$Parser$s('configuration'),
							author$project$Main$slug_))),
					A2(
					elm$url$Url$Parser$map,
					function (slug) {
						return A2(
							author$project$Main$stepGallery,
							model,
							author$project$Gallery$Root$init(slug));
					},
					A2(
						elm$url$Url$Parser$slash,
						elm$url$Url$Parser$s(author$project$Base$base),
						A2(
							elm$url$Url$Parser$slash,
							elm$url$Url$Parser$s('gallery'),
							author$project$Main$slug_)))
				]));
		var _n0 = A2(elm$url$Url$Parser$parse, parser, url);
		if (_n0.$ === 1) {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{r: author$project$Main$NotFound}),
				elm$core$Platform$Cmd$none);
		} else {
			var answer = _n0.a;
			return answer;
		}
	});
var author$project$Main$init = F3(
	function (flags, url, key) {
		return A2(
			author$project$Main$stepUrl,
			url,
			{r: author$project$Main$NotFound, ca: key});
	});
var author$project$Configuration$Groups$InsertAfter$subscriptions = function (model) {
	return author$project$Configuration$Groups$InsertAfter$system.ds(model.z);
};
var author$project$Configuration$Groups$InsertBefore$subscriptions = function (model) {
	return author$project$Configuration$Groups$InsertBefore$system.ds(model.z);
};
var author$project$Configuration$Groups$Root$InsertAfterMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Groups$Root$InsertBeforeMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Groups$Root$SwapMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$Groups$Swap$subscriptions = function (model) {
	return author$project$Configuration$Groups$Swap$system.ds(model.z);
};
var elm$core$Platform$Sub$map = _Platform_map;
var author$project$Configuration$Groups$Root$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		A2(
			elm$core$List$map,
			function (example) {
				switch (example.$) {
					case 0:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Groups$Root$InsertAfterMsg,
							author$project$Configuration$Groups$InsertAfter$subscriptions(mo));
					case 1:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Groups$Root$InsertBeforeMsg,
							author$project$Configuration$Groups$InsertBefore$subscriptions(mo));
					default:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Groups$Root$SwapMsg,
							author$project$Configuration$Groups$Swap$subscriptions(mo));
				}
			},
			model.aP));
};
var author$project$Configuration$Movement$FreeOnDrag$subscriptions = function (model) {
	return author$project$Configuration$Movement$FreeOnDrag$system.ds(model.z);
};
var author$project$Configuration$Movement$FreeOnDrop$subscriptions = function (model) {
	return author$project$Configuration$Movement$FreeOnDrop$system.ds(model.z);
};
var author$project$Configuration$Movement$HorizontalOnDrag$subscriptions = function (model) {
	return author$project$Configuration$Movement$HorizontalOnDrag$system.ds(model.z);
};
var author$project$Configuration$Movement$HorizontalOnDrop$subscriptions = function (model) {
	return author$project$Configuration$Movement$HorizontalOnDrop$system.ds(model.z);
};
var author$project$Configuration$Movement$Root$FreeOnDragMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Movement$Root$FreeOnDropMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Movement$Root$HorizontalOnDragMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$Movement$Root$HorizontalOnDropMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$Movement$Root$VerticalOnDragMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$Movement$Root$VerticalOnDropMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Configuration$Movement$VerticalOnDrag$subscriptions = function (model) {
	return author$project$Configuration$Movement$VerticalOnDrag$system.ds(model.z);
};
var author$project$Configuration$Movement$VerticalOnDrop$subscriptions = function (model) {
	return author$project$Configuration$Movement$VerticalOnDrop$system.ds(model.z);
};
var author$project$Configuration$Movement$Root$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		A2(
			elm$core$List$map,
			function (example) {
				switch (example.$) {
					case 0:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$FreeOnDragMsg,
							author$project$Configuration$Movement$FreeOnDrag$subscriptions(mo));
					case 1:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$FreeOnDropMsg,
							author$project$Configuration$Movement$FreeOnDrop$subscriptions(mo));
					case 2:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$HorizontalOnDragMsg,
							author$project$Configuration$Movement$HorizontalOnDrag$subscriptions(mo));
					case 3:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$HorizontalOnDropMsg,
							author$project$Configuration$Movement$HorizontalOnDrop$subscriptions(mo));
					case 4:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$VerticalOnDragMsg,
							author$project$Configuration$Movement$VerticalOnDrag$subscriptions(mo));
					default:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$Movement$Root$VerticalOnDropMsg,
							author$project$Configuration$Movement$VerticalOnDrop$subscriptions(mo));
				}
			},
			model.aP));
};
var author$project$Configuration$OperationOnDrag$InsertAfter$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$InsertAfter$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$InsertBefore$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$InsertBefore$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$Root$InsertAfterMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$InsertBeforeMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$RotateInMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$RotateOutMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$SwapMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$OperationOnDrag$Root$UnmoveMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Configuration$OperationOnDrag$RotateIn$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$RotateIn$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$RotateOut$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$RotateOut$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$Swap$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$Swap$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$Unmove$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrag$Unmove$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrag$Root$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		A2(
			elm$core$List$map,
			function (example) {
				switch (example.$) {
					case 0:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$InsertAfterMsg,
							author$project$Configuration$OperationOnDrag$InsertAfter$subscriptions(mo));
					case 1:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$InsertBeforeMsg,
							author$project$Configuration$OperationOnDrag$InsertBefore$subscriptions(mo));
					case 2:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$RotateInMsg,
							author$project$Configuration$OperationOnDrag$RotateIn$subscriptions(mo));
					case 3:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$RotateOutMsg,
							author$project$Configuration$OperationOnDrag$RotateOut$subscriptions(mo));
					case 4:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$SwapMsg,
							author$project$Configuration$OperationOnDrag$Swap$subscriptions(mo));
					default:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrag$Root$UnmoveMsg,
							author$project$Configuration$OperationOnDrag$Unmove$subscriptions(mo));
				}
			},
			model.aP));
};
var author$project$Configuration$OperationOnDrop$InsertAfter$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$InsertAfter$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$InsertBefore$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$InsertBefore$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$Root$InsertAfterMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$InsertBeforeMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$RotateInMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$RotateOutMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$SwapMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Configuration$OperationOnDrop$Root$UnmoveMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Configuration$OperationOnDrop$RotateIn$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$RotateIn$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$RotateOut$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$RotateOut$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$Swap$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$Swap$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$Unmove$subscriptions = function (model) {
	return author$project$Configuration$OperationOnDrop$Unmove$system.ds(model.z);
};
var author$project$Configuration$OperationOnDrop$Root$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		A2(
			elm$core$List$map,
			function (example) {
				switch (example.$) {
					case 0:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$InsertAfterMsg,
							author$project$Configuration$OperationOnDrop$InsertAfter$subscriptions(mo));
					case 1:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$InsertBeforeMsg,
							author$project$Configuration$OperationOnDrop$InsertBefore$subscriptions(mo));
					case 2:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$RotateInMsg,
							author$project$Configuration$OperationOnDrop$RotateIn$subscriptions(mo));
					case 3:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$RotateOutMsg,
							author$project$Configuration$OperationOnDrop$RotateOut$subscriptions(mo));
					case 4:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$SwapMsg,
							author$project$Configuration$OperationOnDrop$Swap$subscriptions(mo));
					default:
						var mo = example.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Configuration$OperationOnDrop$Root$UnmoveMsg,
							author$project$Configuration$OperationOnDrop$Unmove$subscriptions(mo));
				}
			},
			model.aP));
};
var author$project$Configuration$Root$GroupsMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Configuration$Root$MovementMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Configuration$Root$OperationOnDragMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Root$OperationOnDropMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Configuration$Root$subscriptions = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Configuration$Root$OperationOnDragMsg,
				author$project$Configuration$OperationOnDrag$Root$subscriptions(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Configuration$Root$OperationOnDropMsg,
				author$project$Configuration$OperationOnDrop$Root$subscriptions(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Configuration$Root$MovementMsg,
				author$project$Configuration$Movement$Root$subscriptions(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Configuration$Root$GroupsMsg,
				author$project$Configuration$Groups$Root$subscriptions(mo));
	}
};
var author$project$Gallery$Hanoi$subscriptions = function (model) {
	return author$project$Gallery$Hanoi$system.ds(model.z);
};
var author$project$Gallery$Puzzle$subscriptions = function (model) {
	return author$project$Gallery$Puzzle$system.ds(model.z);
};
var author$project$Gallery$Root$HanoiMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Gallery$Root$ShapesMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Gallery$Root$TaskBoardMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Gallery$Root$TryOnMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Gallery$Shapes$subscriptions = function (model) {
	return author$project$Gallery$Shapes$system.ds(model.z);
};
var author$project$Gallery$TaskBoard$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				author$project$Gallery$TaskBoard$cardSystem.ds(model.G),
				author$project$Gallery$TaskBoard$columnSystem.ds(model.I)
			]));
};
var author$project$Gallery$TryOn$subscriptions = function (model) {
	return author$project$Gallery$TryOn$system.ds(model.z);
};
var author$project$Gallery$Root$subscriptions = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Gallery$Root$HanoiMsg,
				author$project$Gallery$Hanoi$subscriptions(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Gallery$Root$PuzzleMsg,
				author$project$Gallery$Puzzle$subscriptions(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Gallery$Root$ShapesMsg,
				author$project$Gallery$Shapes$subscriptions(mo));
		case 3:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Gallery$Root$TryOnMsg,
				author$project$Gallery$TryOn$subscriptions(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Gallery$Root$TaskBoardMsg,
				author$project$Gallery$TaskBoard$subscriptions(mo));
	}
};
var author$project$Home$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Introduction$Basic$subscriptions = function (model) {
	return author$project$Introduction$Basic$system.ds(model.z);
};
var author$project$Introduction$BasicElmUI$subscriptions = function (model) {
	return author$project$Introduction$BasicElmUI$system.ds(model.z);
};
var author$project$Introduction$Groups$subscriptions = function (model) {
	return author$project$Introduction$Groups$system.ds(model.z);
};
var author$project$Introduction$Handle$subscriptions = function (model) {
	return author$project$Introduction$Handle$system.ds(model.z);
};
var author$project$Introduction$Independents$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				author$project$Introduction$Independents$redSystem.ds(model.ad),
				author$project$Introduction$Independents$blueSystem.ds(model.R)
			]));
};
var author$project$Introduction$Keyed$subscriptions = function (model) {
	return author$project$Introduction$Keyed$system.ds(model.z);
};
var author$project$Introduction$Margins$subscriptions = function (model) {
	return author$project$Introduction$Margins$system.ds(model.z);
};
var author$project$Introduction$Masonry$subscriptions = function (model) {
	return author$project$Introduction$Masonry$system.ds(model.z);
};
var author$project$Introduction$Resize$subscriptions = function (model) {
	return author$project$Introduction$Resize$system.ds(model.z);
};
var author$project$Introduction$Root$BasicElmUIMsg = function (a) {
	return {$: 1, a: a};
};
var author$project$Introduction$Root$BasicMsg = function (a) {
	return {$: 0, a: a};
};
var author$project$Introduction$Root$GroupsMsg = function (a) {
	return {$: 8, a: a};
};
var author$project$Introduction$Root$HandleMsg = function (a) {
	return {$: 2, a: a};
};
var author$project$Introduction$Root$IndependentsMsg = function (a) {
	return {$: 7, a: a};
};
var author$project$Introduction$Root$KeyedMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Introduction$Root$MarginsMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Introduction$Root$ResizeMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Introduction$Root$subscriptions = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$BasicMsg,
				author$project$Introduction$Basic$subscriptions(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$BasicElmUIMsg,
				author$project$Introduction$BasicElmUI$subscriptions(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$HandleMsg,
				author$project$Introduction$Handle$subscriptions(mo));
		case 3:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$KeyedMsg,
				author$project$Introduction$Keyed$subscriptions(mo));
		case 4:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$MarginsMsg,
				author$project$Introduction$Margins$subscriptions(mo));
		case 5:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$MasonryMsg,
				author$project$Introduction$Masonry$subscriptions(mo));
		case 6:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$ResizeMsg,
				author$project$Introduction$Resize$subscriptions(mo));
		case 7:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$IndependentsMsg,
				author$project$Introduction$Independents$subscriptions(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Introduction$Root$GroupsMsg,
				author$project$Introduction$Groups$subscriptions(mo));
	}
};
var author$project$Main$HomeMsg = function (a) {
	return {$: 3, a: a};
};
var author$project$Main$subscriptions = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			return elm$core$Platform$Sub$none;
		case 1:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$HomeMsg,
				author$project$Home$subscriptions(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$IntroductionMsg,
				author$project$Introduction$Root$subscriptions(mo));
		case 3:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$ConfigurationMsg,
				author$project$Configuration$Root$subscriptions(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GalleryMsg,
				author$project$Gallery$Root$subscriptions(mo));
	}
};
var author$project$Configuration$Groups$InsertAfter$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Configuration$Groups$InsertAfter$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Configuration$Groups$InsertAfter$system.ea(model.z));
	});
var author$project$Configuration$Groups$InsertBefore$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Configuration$Groups$InsertBefore$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Configuration$Groups$InsertBefore$system.ea(model.z));
	});
var author$project$Configuration$Groups$Root$stepInsertAfter = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Groups$Root$InsertAfter(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Groups$Root$InsertAfterMsg, cmds));
};
var author$project$Configuration$Groups$Root$stepInsertBefore = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Groups$Root$InsertBefore(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Groups$Root$InsertBeforeMsg, cmds));
};
var author$project$Configuration$Groups$Root$stepSwap = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Groups$Root$Swap(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Groups$Root$SwapMsg, cmds));
};
var author$project$Configuration$Groups$Swap$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Configuration$Groups$Swap$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Configuration$Groups$Swap$system.ea(model.z));
	});
var elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_n0, _n1) {
			var x = _n0.a;
			var y = _n0.b;
			var xs = _n1.a;
			var ys = _n1.b;
			return _Utils_Tuple2(
				A2(elm$core$List$cons, x, xs),
				A2(elm$core$List$cons, y, ys));
		});
	return A3(
		elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var author$project$Configuration$Groups$Root$update = F2(
	function (message, model) {
		if (!message.$) {
			var id = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{bI: id}),
				elm$core$Platform$Cmd$none);
		} else {
			return function (_n2) {
				var examples = _n2.a;
				var cmds = _n2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aP: examples}),
					elm$core$Platform$Cmd$batch(cmds));
			}(
				elm$core$List$unzip(
					A2(
						elm$core$List$map,
						function (example) {
							var _n1 = _Utils_Tuple2(message, example);
							_n1$3:
							while (true) {
								switch (_n1.b.$) {
									case 0:
										if (_n1.a.$ === 1) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Groups$Root$stepInsertAfter(
												A2(author$project$Configuration$Groups$InsertAfter$update, msg, mo));
										} else {
											break _n1$3;
										}
									case 1:
										if (_n1.a.$ === 2) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Groups$Root$stepInsertBefore(
												A2(author$project$Configuration$Groups$InsertBefore$update, msg, mo));
										} else {
											break _n1$3;
										}
									default:
										if (_n1.a.$ === 3) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Groups$Root$stepSwap(
												A2(author$project$Configuration$Groups$Swap$update, msg, mo));
										} else {
											break _n1$3;
										}
								}
							}
							return _Utils_Tuple2(example, elm$core$Platform$Cmd$none);
						},
						model.aP)));
		}
	});
var author$project$Configuration$Movement$FreeOnDrag$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$FreeOnDrag$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$FreeOnDrag$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, model.O)) : model.O;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$FreeOnDrag$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$FreeOnDrop$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$FreeOnDrop$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$FreeOnDrop$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, _List_Nil)) : _List_Nil;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$FreeOnDrop$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrag$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$HorizontalOnDrag$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$HorizontalOnDrag$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, model.O)) : model.O;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$HorizontalOnDrag$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrop$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$HorizontalOnDrop$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$HorizontalOnDrop$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, _List_Nil)) : _List_Nil;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$HorizontalOnDrop$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$Root$stepFreeOnDrag = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$FreeOnDrag(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$FreeOnDragMsg, cmds));
};
var author$project$Configuration$Movement$Root$stepFreeOnDrop = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$FreeOnDrop(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$FreeOnDropMsg, cmds));
};
var author$project$Configuration$Movement$Root$stepHorizontalOnDrag = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$HorizontalOnDrag(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$HorizontalOnDragMsg, cmds));
};
var author$project$Configuration$Movement$Root$stepHorizontalOnDrop = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$HorizontalOnDrop(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$HorizontalOnDropMsg, cmds));
};
var author$project$Configuration$Movement$Root$stepVerticalOnDrag = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$VerticalOnDrag(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$VerticalOnDragMsg, cmds));
};
var author$project$Configuration$Movement$Root$stepVerticalOnDrop = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$Movement$Root$VerticalOnDrop(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$Movement$Root$VerticalOnDropMsg, cmds));
};
var author$project$Configuration$Movement$VerticalOnDrag$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$VerticalOnDrag$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$VerticalOnDrag$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, model.O)) : model.O;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$VerticalOnDrag$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$VerticalOnDrop$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$Movement$VerticalOnDrop$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			var affected = function () {
				var _n2 = author$project$Configuration$Movement$VerticalOnDrop$system.c_(draggable);
				if (!_n2.$) {
					var dragIndex = _n2.a.cM;
					var dropIndex = _n2.a.ek;
					return (!_Utils_eq(dragIndex, dropIndex)) ? A2(
						elm$core$List$cons,
						dragIndex,
						A2(elm$core$List$cons, dropIndex, _List_Nil)) : _List_Nil;
				} else {
					return model.O;
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: affected, z: draggable, A: items}),
				author$project$Configuration$Movement$VerticalOnDrop$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{O: _List_Nil}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$Movement$Root$update = F2(
	function (message, model) {
		if (!message.$) {
			var id = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{bI: id}),
				elm$core$Platform$Cmd$none);
		} else {
			return function (_n2) {
				var examples = _n2.a;
				var cmds = _n2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aP: examples}),
					elm$core$Platform$Cmd$batch(cmds));
			}(
				elm$core$List$unzip(
					A2(
						elm$core$List$map,
						function (example) {
							var _n1 = _Utils_Tuple2(message, example);
							_n1$6:
							while (true) {
								switch (_n1.b.$) {
									case 0:
										if (_n1.a.$ === 1) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepFreeOnDrag(
												A2(author$project$Configuration$Movement$FreeOnDrag$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 1:
										if (_n1.a.$ === 2) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepFreeOnDrop(
												A2(author$project$Configuration$Movement$FreeOnDrop$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 2:
										if (_n1.a.$ === 3) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepHorizontalOnDrag(
												A2(author$project$Configuration$Movement$HorizontalOnDrag$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 3:
										if (_n1.a.$ === 4) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepHorizontalOnDrop(
												A2(author$project$Configuration$Movement$HorizontalOnDrop$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 4:
										if (_n1.a.$ === 5) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepVerticalOnDrag(
												A2(author$project$Configuration$Movement$VerticalOnDrag$update, msg, mo));
										} else {
											break _n1$6;
										}
									default:
										if (_n1.a.$ === 6) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$Movement$Root$stepVerticalOnDrop(
												A2(author$project$Configuration$Movement$VerticalOnDrop$update, msg, mo));
										} else {
											break _n1$6;
										}
								}
							}
							return _Utils_Tuple2(example, elm$core$Platform$Cmd$none);
						},
						model.aP)));
		}
	});
var author$project$Configuration$OperationOnDrag$InsertAfter$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$InsertAfter$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$InsertAfter$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$InsertAfter$Item, value, author$project$Configuration$OperationOnDrag$InsertAfter$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$InsertBefore$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$InsertBefore$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$InsertBefore$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$InsertBefore$Item, value, author$project$Configuration$OperationOnDrag$InsertBefore$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$Root$stepInsertAfter = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$InsertAfter(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$InsertAfterMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$Root$stepInsertBefore = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$InsertBefore(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$InsertBeforeMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$Root$stepRotateIn = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$RotateIn(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$RotateInMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$Root$stepRotateOut = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$RotateOut(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$RotateOutMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$Root$stepSwap = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$Swap(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$SwapMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$Root$stepUnmove = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrag$Root$Unmove(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrag$Root$UnmoveMsg, cmds));
};
var author$project$Configuration$OperationOnDrag$RotateIn$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$RotateIn$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$RotateIn$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$RotateIn$Item, value, author$project$Configuration$OperationOnDrag$RotateIn$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$RotateOut$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$RotateOut$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$RotateOut$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$RotateOut$Item, value, author$project$Configuration$OperationOnDrag$RotateOut$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$Swap$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$Swap$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$Swap$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$Swap$Item, value, author$project$Configuration$OperationOnDrag$Swap$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$Unmove$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrag$Unmove$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrag$Unmove$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrag$Unmove$Item, value, author$project$Configuration$OperationOnDrag$Unmove$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrag$Root$update = F2(
	function (message, model) {
		if (!message.$) {
			var id = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{bI: id}),
				elm$core$Platform$Cmd$none);
		} else {
			return function (_n2) {
				var examples = _n2.a;
				var cmds = _n2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aP: examples}),
					elm$core$Platform$Cmd$batch(cmds));
			}(
				elm$core$List$unzip(
					A2(
						elm$core$List$map,
						function (example) {
							var _n1 = _Utils_Tuple2(message, example);
							_n1$6:
							while (true) {
								switch (_n1.b.$) {
									case 0:
										if (_n1.a.$ === 1) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepInsertAfter(
												A2(author$project$Configuration$OperationOnDrag$InsertAfter$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 1:
										if (_n1.a.$ === 2) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepInsertBefore(
												A2(author$project$Configuration$OperationOnDrag$InsertBefore$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 2:
										if (_n1.a.$ === 3) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepRotateIn(
												A2(author$project$Configuration$OperationOnDrag$RotateIn$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 3:
										if (_n1.a.$ === 4) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepRotateOut(
												A2(author$project$Configuration$OperationOnDrag$RotateOut$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 4:
										if (_n1.a.$ === 5) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepSwap(
												A2(author$project$Configuration$OperationOnDrag$Swap$update, msg, mo));
										} else {
											break _n1$6;
										}
									default:
										if (_n1.a.$ === 6) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrag$Root$stepUnmove(
												A2(author$project$Configuration$OperationOnDrag$Unmove$update, msg, mo));
										} else {
											break _n1$6;
										}
								}
							}
							return _Utils_Tuple2(example, elm$core$Platform$Cmd$none);
						},
						model.aP)));
		}
	});
var author$project$Configuration$OperationOnDrop$InsertAfter$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$InsertAfter$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$InsertAfter$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$InsertAfter$Item, value, author$project$Configuration$OperationOnDrop$InsertAfter$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$InsertBefore$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$InsertBefore$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$InsertBefore$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$InsertBefore$Item, value, author$project$Configuration$OperationOnDrop$InsertBefore$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$Root$stepInsertAfter = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$InsertAfter(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$InsertAfterMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$Root$stepInsertBefore = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$InsertBefore(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$InsertBeforeMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$Root$stepRotateIn = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$RotateIn(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$RotateInMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$Root$stepRotateOut = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$RotateOut(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$RotateOutMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$Root$stepSwap = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$Swap(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$SwapMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$Root$stepUnmove = function (_n0) {
	var mo = _n0.a;
	var cmds = _n0.b;
	return _Utils_Tuple2(
		author$project$Configuration$OperationOnDrop$Root$Unmove(mo),
		A2(elm$core$Platform$Cmd$map, author$project$Configuration$OperationOnDrop$Root$UnmoveMsg, cmds));
};
var author$project$Configuration$OperationOnDrop$RotateIn$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$RotateIn$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$RotateIn$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$RotateIn$Item, value, author$project$Configuration$OperationOnDrop$RotateIn$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$RotateOut$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$RotateOut$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$RotateOut$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$RotateOut$Item, value, author$project$Configuration$OperationOnDrop$RotateOut$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$Swap$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$Swap$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$Swap$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$Swap$Item, value, author$project$Configuration$OperationOnDrop$Swap$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$Unmove$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Configuration$OperationOnDrop$Unmove$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Configuration$OperationOnDrop$Unmove$system.ea(model.z));
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$map,
							function (_n2) {
								var value = _n2.co;
								return A2(author$project$Configuration$OperationOnDrop$Unmove$Item, value, author$project$Configuration$OperationOnDrop$Unmove$baseColor);
							},
							model.A)
					}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Configuration$OperationOnDrop$Root$update = F2(
	function (message, model) {
		if (!message.$) {
			var id = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{bI: id}),
				elm$core$Platform$Cmd$none);
		} else {
			return function (_n2) {
				var examples = _n2.a;
				var cmds = _n2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aP: examples}),
					elm$core$Platform$Cmd$batch(cmds));
			}(
				elm$core$List$unzip(
					A2(
						elm$core$List$map,
						function (example) {
							var _n1 = _Utils_Tuple2(message, example);
							_n1$6:
							while (true) {
								switch (_n1.b.$) {
									case 0:
										if (_n1.a.$ === 1) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepInsertAfter(
												A2(author$project$Configuration$OperationOnDrop$InsertAfter$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 1:
										if (_n1.a.$ === 2) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepInsertBefore(
												A2(author$project$Configuration$OperationOnDrop$InsertBefore$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 2:
										if (_n1.a.$ === 3) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepRotateIn(
												A2(author$project$Configuration$OperationOnDrop$RotateIn$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 3:
										if (_n1.a.$ === 4) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepRotateOut(
												A2(author$project$Configuration$OperationOnDrop$RotateOut$update, msg, mo));
										} else {
											break _n1$6;
										}
									case 4:
										if (_n1.a.$ === 5) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepSwap(
												A2(author$project$Configuration$OperationOnDrop$Swap$update, msg, mo));
										} else {
											break _n1$6;
										}
									default:
										if (_n1.a.$ === 6) {
											var msg = _n1.a.a;
											var mo = _n1.b.a;
											return author$project$Configuration$OperationOnDrop$Root$stepUnmove(
												A2(author$project$Configuration$OperationOnDrop$Unmove$update, msg, mo));
										} else {
											break _n1$6;
										}
								}
							}
							return _Utils_Tuple2(example, elm$core$Platform$Cmd$none);
						},
						model.aP)));
		}
	});
var author$project$Configuration$Root$stepGroups = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Configuration$Root$Groups(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Configuration$Root$GroupsMsg, cmds));
	});
var author$project$Configuration$Root$stepMovement = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Configuration$Root$Movement(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Configuration$Root$MovementMsg, cmds));
	});
var author$project$Configuration$Root$stepOperationOnDrag = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Configuration$Root$OperationOnDrag(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Configuration$Root$OperationOnDragMsg, cmds));
	});
var author$project$Configuration$Root$stepOperationOnDrop = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Configuration$Root$OperationOnDrop(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Configuration$Root$OperationOnDropMsg, cmds));
	});
var author$project$Configuration$Root$update = F2(
	function (message, model) {
		var _n0 = _Utils_Tuple2(message, model.r);
		_n0$4:
		while (true) {
			switch (_n0.a.$) {
				case 0:
					if (!_n0.b.$) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Configuration$Root$stepOperationOnDrag,
							model,
							A2(author$project$Configuration$OperationOnDrag$Root$update, msg, mo));
					} else {
						break _n0$4;
					}
				case 1:
					if (_n0.b.$ === 1) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Configuration$Root$stepOperationOnDrop,
							model,
							A2(author$project$Configuration$OperationOnDrop$Root$update, msg, mo));
					} else {
						break _n0$4;
					}
				case 2:
					if (_n0.b.$ === 2) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Configuration$Root$stepMovement,
							model,
							A2(author$project$Configuration$Movement$Root$update, msg, mo));
					} else {
						break _n0$4;
					}
				default:
					if (_n0.b.$ === 3) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Configuration$Root$stepGroups,
							model,
							A2(author$project$Configuration$Groups$Root$update, msg, mo));
					} else {
						break _n0$4;
					}
			}
		}
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var author$project$Gallery$Hanoi$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Gallery$Hanoi$system.dz, msg, model.z, model.U);
		var draggable = _n1.a;
		var disks = _n1.b;
		var solved = A2(
			elm$core$List$all,
			function (disk) {
				return disk.bV === 'transparent';
			},
			A2(elm$core$List$take, 3, disks));
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{U: disks, z: draggable, bm: solved}),
			author$project$Gallery$Hanoi$system.ea(model.z));
	});
var elm$core$Basics$modBy = _Basics_modBy;
var elm$core$List$sortBy = _List_sortBy;
var author$project$Gallery$Puzzle$update = F2(
	function (message, model) {
		if (!message.$) {
			var shuffled = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A2(
							elm$core$List$sortBy,
							function ($) {
								return $.t;
							},
							A2(
								elm$core$List$indexedMap,
								F2(
									function (i, _n1) {
										var value = _n1.co;
										var color = _n1.b3;
										var _n2 = A2(elm$core$Basics$modBy, 4, i);
										switch (_n2) {
											case 0:
												return A3(author$project$Gallery$Puzzle$Item, 0, value, color);
											case 1:
												return A3(author$project$Gallery$Puzzle$Item, 1, value, color);
											case 2:
												return A3(author$project$Gallery$Puzzle$Item, 2, value, color);
											case 3:
												return A3(author$project$Gallery$Puzzle$Item, 3, value, color);
											default:
												return A3(author$project$Gallery$Puzzle$Item, 3, value, color);
										}
									}),
								shuffled))
					}),
				elm$core$Platform$Cmd$none);
		} else {
			var msg = message.a;
			var _n3 = A3(author$project$Gallery$Puzzle$system.dz, msg, model.z, model.A);
			var draggable = _n3.a;
			var items = _n3.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Gallery$Puzzle$system.ea(model.z));
		}
	});
var author$project$Gallery$Root$stepHanoi = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Gallery$Root$Hanoi(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$HanoiMsg, cmds));
	});
var author$project$Gallery$Root$stepPuzzle = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Gallery$Root$Puzzle(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$PuzzleMsg, cmds));
	});
var author$project$Gallery$Root$stepShapes = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Gallery$Root$Shapes(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$ShapesMsg, cmds));
	});
var author$project$Gallery$Root$stepTaskBoard = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Gallery$Root$TaskBoard(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$TaskBoardMsg, cmds));
	});
var author$project$Gallery$Root$stepTryOn = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Gallery$Root$TryOn(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Gallery$Root$TryOnMsg, cmds));
	});
var author$project$Gallery$Shapes$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Gallery$Shapes$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Gallery$Shapes$system.ea(model.z));
	});
var author$project$Gallery$TaskBoard$gatherByActivity = function (cards) {
	return A3(
		elm$core$List$foldr,
		F2(
			function (x, acc) {
				if (!acc.b) {
					return _List_fromArray(
						[
							_List_fromArray(
							[x])
						]);
				} else {
					if (acc.a.b) {
						var _n1 = acc.a;
						var y = _n1.a;
						var restOfGroup = _n1.b;
						var groups = acc.b;
						return _Utils_eq(x.v, y.v) ? A2(
							elm$core$List$cons,
							A2(
								elm$core$List$cons,
								x,
								A2(elm$core$List$cons, y, restOfGroup)),
							groups) : A2(
							elm$core$List$cons,
							_List_fromArray(
								[x]),
							acc);
					} else {
						return acc;
					}
				}
			}),
		_List_Nil,
		cards);
};
var author$project$Gallery$TaskBoard$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Gallery$TaskBoard$cardSystem.dz, msg, model.G, model.H);
			var cardDraggable = _n1.a;
			var cards = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{G: cardDraggable, H: cards}),
				author$project$Gallery$TaskBoard$cardSystem.ea(model.G));
		} else {
			var msg = message.a;
			var _n2 = A3(
				author$project$Gallery$TaskBoard$columnSystem.dz,
				msg,
				model.I,
				author$project$Gallery$TaskBoard$gatherByActivity(model.H));
			var columnDraggable = _n2.a;
			var columns = _n2.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						H: elm$core$List$concat(columns),
						I: columnDraggable
					}),
				author$project$Gallery$TaskBoard$columnSystem.ea(model.I));
		}
	});
var author$project$Gallery$TryOn$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Gallery$TryOn$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Gallery$TryOn$system.ea(model.z));
	});
var author$project$Gallery$Root$update = F2(
	function (message, model) {
		var _n0 = _Utils_Tuple2(message, model.r);
		_n0$5:
		while (true) {
			switch (_n0.a.$) {
				case 0:
					if (!_n0.b.$) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Gallery$Root$stepHanoi,
							model,
							A2(author$project$Gallery$Hanoi$update, msg, mo));
					} else {
						break _n0$5;
					}
				case 1:
					if (_n0.b.$ === 1) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Gallery$Root$stepPuzzle,
							model,
							A2(author$project$Gallery$Puzzle$update, msg, mo));
					} else {
						break _n0$5;
					}
				case 2:
					if (_n0.b.$ === 2) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Gallery$Root$stepShapes,
							model,
							A2(author$project$Gallery$Shapes$update, msg, mo));
					} else {
						break _n0$5;
					}
				case 3:
					if (_n0.b.$ === 3) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Gallery$Root$stepTryOn,
							model,
							A2(author$project$Gallery$TryOn$update, msg, mo));
					} else {
						break _n0$5;
					}
				default:
					if (_n0.b.$ === 4) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Gallery$Root$stepTaskBoard,
							model,
							A2(author$project$Gallery$TaskBoard$update, msg, mo));
					} else {
						break _n0$5;
					}
			}
		}
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var author$project$Home$update = F2(
	function (message, model) {
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var author$project$Introduction$Basic$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Basic$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Introduction$Basic$system.ea(model.z));
	});
var author$project$Introduction$BasicElmUI$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$BasicElmUI$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Introduction$BasicElmUI$system.ea(model.z));
	});
var author$project$Introduction$Groups$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Groups$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Introduction$Groups$system.ea(model.z));
	});
var author$project$Introduction$Handle$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Handle$system.dz, msg, model.z, model.aT);
		var draggable = _n1.a;
		var fruits = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, aT: fruits}),
			author$project$Introduction$Handle$system.ea(model.z));
	});
var author$project$Introduction$Independents$update = F2(
	function (message, model) {
		if (!message.$) {
			var msg = message.a;
			var _n1 = A3(author$project$Introduction$Independents$redSystem.dz, msg, model.ad, model.a0);
			var redDraggable = _n1.a;
			var reds = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{ad: redDraggable, a0: reds}),
				author$project$Introduction$Independents$redSystem.ea(model.ad));
		} else {
			var msg = message.a;
			var _n2 = A3(author$project$Introduction$Independents$blueSystem.dz, msg, model.R, model.aI);
			var blueDraggable = _n2.a;
			var blues = _n2.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{R: blueDraggable, aI: blues}),
				author$project$Introduction$Independents$blueSystem.ea(model.R));
		}
	});
var author$project$Introduction$Keyed$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Keyed$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Introduction$Keyed$system.ea(model.z));
	});
var author$project$Introduction$Margins$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Margins$system.dz, msg, model.z, model.A);
		var draggable = _n1.a;
		var items = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{z: draggable, A: items}),
			author$project$Introduction$Margins$system.ea(model.z));
	});
var author$project$Introduction$Masonry$Item = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var author$project$Introduction$Masonry$update = F2(
	function (message, model) {
		if (!message.$) {
			var widths = message.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						A: A3(
							elm$core$List$map2,
							F2(
								function (color, width) {
									return A2(author$project$Introduction$Masonry$Item, color, width);
								}),
							author$project$Introduction$Masonry$colors,
							widths)
					}),
				elm$core$Platform$Cmd$none);
		} else {
			var msg = message.a;
			var _n1 = A3(author$project$Introduction$Masonry$system.dz, msg, model.z, model.A);
			var draggable = _n1.a;
			var items = _n1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{z: draggable, A: items}),
				author$project$Introduction$Masonry$system.ea(model.z));
		}
	});
var author$project$Introduction$Resize$update = F2(
	function (message, model) {
		var msg = message;
		var _n1 = A3(author$project$Introduction$Resize$system.dz, msg, model.z, model.aM);
		var draggable = _n1.a;
		var colors = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{aM: colors, z: draggable}),
			author$project$Introduction$Resize$system.ea(model.z));
	});
var author$project$Introduction$Root$stepBasic = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Basic(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$BasicMsg, cmds));
	});
var author$project$Introduction$Root$stepBasicElmUI = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$BasicElmUI(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$BasicElmUIMsg, cmds));
	});
var author$project$Introduction$Root$stepGroups = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Groups(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$GroupsMsg, cmds));
	});
var author$project$Introduction$Root$stepHandle = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Handle(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$HandleMsg, cmds));
	});
var author$project$Introduction$Root$stepIndependents = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Independents(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$IndependentsMsg, cmds));
	});
var author$project$Introduction$Root$stepKeyed = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Keyed(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$KeyedMsg, cmds));
	});
var author$project$Introduction$Root$stepMargins = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Margins(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$MarginsMsg, cmds));
	});
var author$project$Introduction$Root$stepMasonry = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Masonry(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$MasonryMsg, cmds));
	});
var author$project$Introduction$Root$stepResize = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Introduction$Root$Resize(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Introduction$Root$ResizeMsg, cmds));
	});
var author$project$Introduction$Root$update = F2(
	function (message, model) {
		var _n0 = _Utils_Tuple2(message, model.r);
		_n0$9:
		while (true) {
			switch (_n0.a.$) {
				case 0:
					if (!_n0.b.$) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepBasic,
							model,
							A2(author$project$Introduction$Basic$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 1:
					if (_n0.b.$ === 1) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepBasicElmUI,
							model,
							A2(author$project$Introduction$BasicElmUI$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 2:
					if (_n0.b.$ === 2) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepHandle,
							model,
							A2(author$project$Introduction$Handle$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 3:
					if (_n0.b.$ === 3) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepKeyed,
							model,
							A2(author$project$Introduction$Keyed$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 4:
					if (_n0.b.$ === 4) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepMargins,
							model,
							A2(author$project$Introduction$Margins$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 5:
					if (_n0.b.$ === 5) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepMasonry,
							model,
							A2(author$project$Introduction$Masonry$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 6:
					if (_n0.b.$ === 6) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepResize,
							model,
							A2(author$project$Introduction$Resize$update, msg, mo));
					} else {
						break _n0$9;
					}
				case 7:
					if (_n0.b.$ === 7) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepIndependents,
							model,
							A2(author$project$Introduction$Independents$update, msg, mo));
					} else {
						break _n0$9;
					}
				default:
					if (_n0.b.$ === 8) {
						var msg = _n0.a.a;
						var mo = _n0.b.a;
						return A2(
							author$project$Introduction$Root$stepGroups,
							model,
							A2(author$project$Introduction$Groups$update, msg, mo));
					} else {
						break _n0$9;
					}
			}
		}
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var author$project$Main$NoOp = {$: 0};
var elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var elm$browser$Browser$Dom$setViewportOf = _Browser_setViewportOf;
var author$project$Main$jumpToTop = function (id) {
	return A2(
		elm$core$Task$attempt,
		function (_n1) {
			return author$project$Main$NoOp;
		},
		A2(
			elm$core$Task$andThen,
			function (_n0) {
				return A3(elm$browser$Browser$Dom$setViewportOf, id, 0, 0);
			},
			elm$browser$Browser$Dom$getViewportOf(id)));
};
var author$project$Main$Home = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$stepHome = F2(
	function (model, _n0) {
		var mo = _n0.a;
		var cmds = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					r: author$project$Main$Home(mo)
				}),
			A2(elm$core$Platform$Cmd$map, author$project$Main$HomeMsg, cmds));
	});
var elm$browser$Browser$Navigation$load = _Browser_load;
var elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + elm$core$String$fromInt(port_));
		}
	});
var elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var elm$url$Url$toString = function (url) {
	var http = function () {
		var _n0 = url.di;
		if (!_n0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		elm$url$Url$addPrefixed,
		'#',
		url.cS,
		A3(
			elm$url$Url$addPrefixed,
			'?',
			url.dj,
			_Utils_ap(
				A2(
					elm$url$Url$addPort,
					url.df,
					_Utils_ap(http, url.cW)),
				url.dd)));
};
var author$project$Main$update = F2(
	function (message, model) {
		switch (message.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var urlRequest = message.a;
				if (!urlRequest.$) {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									A2(
									elm$browser$Browser$Navigation$pushUrl,
									model.ca,
									elm$url$Url$toString(url)),
									author$project$Main$jumpToTop('main')
								])));
				} else {
					var href = urlRequest.a;
					return _Utils_Tuple2(
						model,
						elm$browser$Browser$Navigation$load(href));
				}
			case 2:
				var url = message.a;
				return A2(author$project$Main$stepUrl, url, model);
			case 3:
				var msg = message.a;
				var _n2 = model.r;
				if (_n2.$ === 1) {
					var mo = _n2.a;
					return A2(
						author$project$Main$stepHome,
						model,
						A2(author$project$Home$update, msg, mo));
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 4:
				var msg = message.a;
				var _n3 = model.r;
				if (_n3.$ === 2) {
					var mo = _n3.a;
					return A2(
						author$project$Main$stepIntroduction,
						model,
						A2(author$project$Introduction$Root$update, msg, mo));
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 5:
				var msg = message.a;
				var _n4 = model.r;
				if (_n4.$ === 3) {
					var mo = _n4.a;
					return A2(
						author$project$Main$stepConfiguration,
						model,
						A2(author$project$Configuration$Root$update, msg, mo));
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			default:
				var msg = message.a;
				var _n5 = model.r;
				if (_n5.$ === 4) {
					var mo = _n5.a;
					return A2(
						author$project$Main$stepGallery,
						model,
						A2(author$project$Gallery$Root$update, msg, mo));
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
		}
	});
var author$project$Configuration$Groups$Root$url = function (id) {
	switch (id) {
		case 0:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Groups/InsertAfter.elm';
		case 1:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Groups/InsertBefore.elm';
		case 2:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Groups/Swap.elm';
		default:
			return '';
	}
};
var author$project$Configuration$Movement$Root$url = function (id) {
	switch (id) {
		case 0:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/FreeOnDrag.elm';
		case 1:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/FreeOnDrop.elm';
		case 2:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/HorizontalOnDrag.elm';
		case 3:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/HorizontalOnDrop.elm';
		case 4:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/VerticalOnDrag.elm';
		case 5:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/Movement/VerticalOnDrop.elm';
		default:
			return '';
	}
};
var author$project$Configuration$OperationOnDrag$Root$url = function (id) {
	switch (id) {
		case 0:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/InsertAfter.elm';
		case 1:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/InsertBefore.elm';
		case 2:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/RotateIn.elm';
		case 3:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/RotateOut.elm';
		case 4:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/Swap.elm';
		case 5:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrag/Unmove.elm';
		default:
			return '';
	}
};
var author$project$Configuration$OperationOnDrop$Root$url = function (id) {
	switch (id) {
		case 0:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/InsertAfter.elm';
		case 1:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/InsertBefore.elm';
		case 2:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/RotateIn.elm';
		case 3:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/RotateOut.elm';
		case 4:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/Swap.elm';
		case 5:
			return 'https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Configuration/OperationOnDrop/Unmove.elm';
		default:
			return '';
	}
};
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var author$project$CustomElement$elmCode = F2(
	function (attrs, elems) {
		return A3(elm$html$Html$node, 'elm-code', attrs, elems);
	});
var elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$property = elm$virtual_dom$VirtualDom$property;
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$CustomElement$href = function (url) {
	return A2(
		elm$html$Html$Attributes$property,
		'href',
		elm$json$Json$Encode$string(url));
};
var author$project$Configuration$Root$toCode = function (url) {
	return A2(
		author$project$CustomElement$elmCode,
		_List_fromArray(
			[
				author$project$CustomElement$href(url)
			]),
		_List_Nil);
};
var author$project$Configuration$Root$codeView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return author$project$Configuration$Root$toCode(
				author$project$Configuration$OperationOnDrag$Root$url(mo.bI));
		case 1:
			var mo = _n0.a;
			return author$project$Configuration$Root$toCode(
				author$project$Configuration$OperationOnDrop$Root$url(mo.bI));
		case 2:
			var mo = _n0.a;
			return author$project$Configuration$Root$toCode(
				author$project$Configuration$Movement$Root$url(mo.bI));
		default:
			var mo = _n0.a;
			return author$project$Configuration$Root$toCode(
				author$project$Configuration$Groups$Root$url(mo.bI));
	}
};
var author$project$Configuration$Groups$Root$LinkClicked = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Groups$InsertAfter$calculateOffset = F3(
	function (index, group, list) {
		calculateOffset:
		while (true) {
			if (!list.b) {
				return 0;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x.t, group)) {
					return index;
				} else {
					var $temp$index = index + 1,
						$temp$group = group,
						$temp$list = xs;
					index = $temp$index;
					group = $temp$group;
					list = $temp$list;
					continue calculateOffset;
				}
			}
		}
	});
var author$project$Configuration$Groups$InsertAfter$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'end'),
		A2(elm$html$Html$Attributes$style, 'padding-bottom', '3em')
	]);
var author$project$Configuration$Groups$InsertAfter$draggableItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var author$project$Configuration$Groups$InsertAfter$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px'),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'margin-right', '2em'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'background', color)
		]);
};
var author$project$Configuration$Groups$InsertAfter$maybeDraggedItem = function (_n0) {
	var draggable = _n0.z;
	var items = _n0.A;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, items));
		},
		author$project$Configuration$Groups$InsertAfter$system.c_(draggable));
};
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var author$project$Configuration$Groups$InsertAfter$draggedItemView = function (model) {
	var _n0 = author$project$Configuration$Groups$InsertAfter$maybeDraggedItem(model);
	if (!_n0.$) {
		var value = _n0.a.co;
		var color = _n0.a.b3;
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				author$project$Configuration$Groups$InsertAfter$itemStyles(color),
				_Utils_ap(
					author$project$Configuration$Groups$InsertAfter$draggableItemStyles,
					author$project$Configuration$Groups$InsertAfter$system.ej(model.z))),
			_List_fromArray(
				[
					elm$html$Html$text(value)
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Configuration$Groups$InsertAfter$auxiliaryItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
		A2(elm$html$Html$Attributes$style, 'box-sizing', 'border-box'),
		A2(elm$html$Html$Attributes$style, 'margin-right', '2em'),
		A2(elm$html$Html$Attributes$style, 'width', 'auto'),
		A2(elm$html$Html$Attributes$style, 'height', '50px'),
		A2(elm$html$Html$Attributes$style, 'min-width', '50px'),
		A2(elm$html$Html$Attributes$style, 'border', '3px dashed dimgray'),
		A2(elm$html$Html$Attributes$style, 'background', 'transparent')
	]);
var author$project$Configuration$Groups$InsertAfter$gray = 'dimgray';
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var author$project$Configuration$Groups$InsertAfter$itemView = F4(
	function (model, offset, localIndex, _n0) {
		var group = _n0.t;
		var value = _n0.co;
		var color = _n0.b3;
		var globalIndex = localIndex + offset;
		var itemId = 'start-' + elm$core$String$fromInt(globalIndex);
		var _n1 = _Utils_Tuple2(
			author$project$Configuration$Groups$InsertAfter$system.c_(model.z),
			author$project$Configuration$Groups$InsertAfter$maybeDraggedItem(model));
		if ((!_n1.a.$) && (!_n1.b.$)) {
			var dragIndex = _n1.a.a.cM;
			var dragItem = _n1.b.a;
			return ((value === '') && (!_Utils_eq(group, dragItem.t))) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertAfter$auxiliaryItemStyles,
						A2(author$project$Configuration$Groups$InsertAfter$system.cN, globalIndex, itemId))),
				_List_Nil) : (((value === '') && _Utils_eq(group, dragItem.t)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertAfter$auxiliaryItemStyles),
				_List_Nil) : ((!_Utils_eq(globalIndex, dragIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertAfter$itemStyles(color),
						A2(author$project$Configuration$Groups$InsertAfter$system.cN, globalIndex, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertAfter$itemStyles(author$project$Configuration$Groups$InsertAfter$gray)),
				_List_Nil)));
		} else {
			return (value === '') ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertAfter$auxiliaryItemStyles),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertAfter$itemStyles(color),
						_Utils_ap(
							author$project$Configuration$Groups$InsertAfter$draggableItemStyles,
							A2(author$project$Configuration$Groups$InsertAfter$system.ei, globalIndex, itemId)))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$Groups$InsertAfter$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'width', '700px')
	]);
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$html$Html$section = _VirtualDom_node('section');
var author$project$Configuration$Groups$InsertAfter$view = function (model) {
	return A2(
		elm$html$Html$section,
		author$project$Configuration$Groups$InsertAfter$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertAfter$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Groups$InsertAfter$itemView, model, 0),
					A2(
						elm$core$List$filter,
						function (_n0) {
							var group = _n0.t;
							return !group;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertAfter$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$InsertAfter$itemView,
						model,
						A3(author$project$Configuration$Groups$InsertAfter$calculateOffset, 0, 1, model.A)),
					A2(
						elm$core$List$filter,
						function (_n1) {
							var group = _n1.t;
							return group === 1;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertAfter$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$InsertAfter$itemView,
						model,
						A3(author$project$Configuration$Groups$InsertAfter$calculateOffset, 0, 2, model.A)),
					A2(
						elm$core$List$filter,
						function (_n2) {
							var group = _n2.t;
							return group === 2;
						},
						model.A))),
				author$project$Configuration$Groups$InsertAfter$draggedItemView(model)
			]));
};
var author$project$Configuration$Groups$InsertBefore$calculateOffset = F3(
	function (index, group, list) {
		calculateOffset:
		while (true) {
			if (!list.b) {
				return 0;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x.t, group)) {
					return index;
				} else {
					var $temp$index = index + 1,
						$temp$group = group,
						$temp$list = xs;
					index = $temp$index;
					group = $temp$group;
					list = $temp$list;
					continue calculateOffset;
				}
			}
		}
	});
var author$project$Configuration$Groups$InsertBefore$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'end'),
		A2(elm$html$Html$Attributes$style, 'padding-bottom', '3em')
	]);
var author$project$Configuration$Groups$InsertBefore$draggableItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var author$project$Configuration$Groups$InsertBefore$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px'),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'margin-right', '2em'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'background', color)
		]);
};
var author$project$Configuration$Groups$InsertBefore$maybeDraggedItem = function (_n0) {
	var draggable = _n0.z;
	var items = _n0.A;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, items));
		},
		author$project$Configuration$Groups$InsertBefore$system.c_(draggable));
};
var author$project$Configuration$Groups$InsertBefore$draggedItemView = function (model) {
	var _n0 = author$project$Configuration$Groups$InsertBefore$maybeDraggedItem(model);
	if (!_n0.$) {
		var value = _n0.a.co;
		var color = _n0.a.b3;
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				author$project$Configuration$Groups$InsertBefore$itemStyles(color),
				_Utils_ap(
					author$project$Configuration$Groups$InsertBefore$draggableItemStyles,
					author$project$Configuration$Groups$InsertBefore$system.ej(model.z))),
			_List_fromArray(
				[
					elm$html$Html$text(value)
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Configuration$Groups$InsertBefore$auxiliaryItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
		A2(elm$html$Html$Attributes$style, 'box-sizing', 'border-box'),
		A2(elm$html$Html$Attributes$style, 'margin-right', '2em'),
		A2(elm$html$Html$Attributes$style, 'width', 'auto'),
		A2(elm$html$Html$Attributes$style, 'height', '50px'),
		A2(elm$html$Html$Attributes$style, 'min-width', '50px'),
		A2(elm$html$Html$Attributes$style, 'border', '3px dashed dimgray'),
		A2(elm$html$Html$Attributes$style, 'background', 'transparent')
	]);
var author$project$Configuration$Groups$InsertBefore$gray = 'dimgray';
var author$project$Configuration$Groups$InsertBefore$itemView = F4(
	function (model, offset, localIndex, _n0) {
		var group = _n0.t;
		var value = _n0.co;
		var color = _n0.b3;
		var globalIndex = localIndex + offset;
		var itemId = 'end-' + elm$core$String$fromInt(globalIndex);
		var _n1 = _Utils_Tuple2(
			author$project$Configuration$Groups$InsertBefore$system.c_(model.z),
			author$project$Configuration$Groups$InsertBefore$maybeDraggedItem(model));
		if ((!_n1.a.$) && (!_n1.b.$)) {
			var dragIndex = _n1.a.a.cM;
			var dragItem = _n1.b.a;
			return ((value === '') && (!_Utils_eq(group, dragItem.t))) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertBefore$auxiliaryItemStyles,
						A2(author$project$Configuration$Groups$InsertBefore$system.cN, globalIndex, itemId))),
				_List_Nil) : (((value === '') && _Utils_eq(group, dragItem.t)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertBefore$auxiliaryItemStyles),
				_List_Nil) : ((!_Utils_eq(globalIndex, dragIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertBefore$itemStyles(color),
						A2(author$project$Configuration$Groups$InsertBefore$system.cN, globalIndex, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertBefore$itemStyles(author$project$Configuration$Groups$InsertBefore$gray)),
				_List_Nil)));
		} else {
			return (value === '') ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$InsertBefore$auxiliaryItemStyles),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$InsertBefore$itemStyles(color),
						_Utils_ap(
							author$project$Configuration$Groups$InsertBefore$draggableItemStyles,
							A2(author$project$Configuration$Groups$InsertBefore$system.ei, globalIndex, itemId)))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$Groups$InsertBefore$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'width', '700px')
	]);
var author$project$Configuration$Groups$InsertBefore$view = function (model) {
	return A2(
		elm$html$Html$section,
		author$project$Configuration$Groups$InsertBefore$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertBefore$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Groups$InsertBefore$itemView, model, 0),
					A2(
						elm$core$List$filter,
						function (_n0) {
							var group = _n0.t;
							return !group;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertBefore$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$InsertBefore$itemView,
						model,
						A3(author$project$Configuration$Groups$InsertBefore$calculateOffset, 0, 1, model.A)),
					A2(
						elm$core$List$filter,
						function (_n1) {
							var group = _n1.t;
							return group === 1;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$InsertBefore$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$InsertBefore$itemView,
						model,
						A3(author$project$Configuration$Groups$InsertBefore$calculateOffset, 0, 2, model.A)),
					A2(
						elm$core$List$filter,
						function (_n2) {
							var group = _n2.t;
							return group === 2;
						},
						model.A))),
				author$project$Configuration$Groups$InsertBefore$draggedItemView(model)
			]));
};
var author$project$Configuration$Groups$Swap$calculateOffset = F3(
	function (index, group, list) {
		calculateOffset:
		while (true) {
			if (!list.b) {
				return 0;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x.t, group)) {
					return index;
				} else {
					var $temp$index = index + 1,
						$temp$group = group,
						$temp$list = xs;
					index = $temp$index;
					group = $temp$group;
					list = $temp$list;
					continue calculateOffset;
				}
			}
		}
	});
var author$project$Configuration$Groups$Swap$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'end'),
		A2(elm$html$Html$Attributes$style, 'padding-bottom', '3em')
	]);
var author$project$Configuration$Groups$Swap$draggableItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var author$project$Configuration$Groups$Swap$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px'),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'margin-right', '2em'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'background', color)
		]);
};
var author$project$Configuration$Groups$Swap$draggedItemView = function (model) {
	var maybeDraggedItem = A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, model.A));
		},
		author$project$Configuration$Groups$Swap$system.c_(model.z));
	if (!maybeDraggedItem.$) {
		var value = maybeDraggedItem.a.co;
		var color = maybeDraggedItem.a.b3;
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				author$project$Configuration$Groups$Swap$itemStyles(color),
				_Utils_ap(
					author$project$Configuration$Groups$Swap$draggableItemStyles,
					author$project$Configuration$Groups$Swap$system.ej(model.z))),
			_List_fromArray(
				[
					elm$html$Html$text(value)
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Configuration$Groups$Swap$gray = 'dimgray';
var author$project$Configuration$Groups$Swap$itemView = F4(
	function (model, offset, localIndex, _n0) {
		var group = _n0.t;
		var value = _n0.co;
		var color = _n0.b3;
		var globalIndex = localIndex + offset;
		var itemId = 'none-' + elm$core$String$fromInt(globalIndex);
		var _n1 = author$project$Configuration$Groups$Swap$system.c_(model.z);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			return (!_Utils_eq(globalIndex, dragIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$Swap$itemStyles(color),
						A2(author$project$Configuration$Groups$Swap$system.cN, globalIndex, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Configuration$Groups$Swap$itemStyles(author$project$Configuration$Groups$Swap$gray)),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Configuration$Groups$Swap$itemStyles(color),
						_Utils_ap(
							author$project$Configuration$Groups$Swap$draggableItemStyles,
							A2(author$project$Configuration$Groups$Swap$system.ei, globalIndex, itemId)))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$Groups$Swap$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'width', '700px'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center')
	]);
var author$project$Configuration$Groups$Swap$view = function (model) {
	return A2(
		elm$html$Html$section,
		author$project$Configuration$Groups$Swap$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$Swap$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Groups$Swap$itemView, model, 0),
					A2(
						elm$core$List$filter,
						function (_n0) {
							var group = _n0.t;
							return !group;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$Swap$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$Swap$itemView,
						model,
						A3(author$project$Configuration$Groups$Swap$calculateOffset, 0, 1, model.A)),
					A2(
						elm$core$List$filter,
						function (_n1) {
							var group = _n1.t;
							return group === 1;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Configuration$Groups$Swap$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Configuration$Groups$Swap$itemView,
						model,
						A3(author$project$Configuration$Groups$Swap$calculateOffset, 0, 2, model.A)),
					A2(
						elm$core$List$filter,
						function (_n2) {
							var group = _n2.t;
							return group === 2;
						},
						model.A))),
				author$project$Configuration$Groups$Swap$draggedItemView(model)
			]));
};
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var author$project$Configuration$Groups$Root$demoView = function (example) {
	switch (example.$) {
		case 0:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Groups$Root$InsertAfterMsg,
				author$project$Configuration$Groups$InsertAfter$view(mo));
		case 1:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Groups$Root$InsertBeforeMsg,
				author$project$Configuration$Groups$InsertBefore$view(mo));
		default:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Groups$Root$SwapMsg,
				author$project$Configuration$Groups$Swap$view(mo));
	}
};
var author$project$Configuration$Groups$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {bp: 'Insert after'};
		case 1:
			return {bp: 'Insert before'};
		default:
			return {bp: 'Swap'};
	}
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Configuration$Groups$Root$demoWrapperView = F3(
	function (currentId, id, example) {
		var title = A2(
			elm$core$Basics$composeR,
			author$project$Configuration$Groups$Root$info,
			function ($) {
				return $.bp;
			})(example);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
					A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2(elm$html$Html$Attributes$style, 'margin', '4em 0')
				]),
			_List_fromArray(
				[
					author$project$Configuration$Groups$Root$demoView(example),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('link'),
							elm$html$Html$Events$onClick(
							author$project$Configuration$Groups$Root$LinkClicked(id))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(title)
						]))
				]));
	});
var author$project$Configuration$Groups$Root$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		A2(
			elm$core$List$indexedMap,
			author$project$Configuration$Groups$Root$demoWrapperView(model.bI),
			model.aP));
};
var author$project$Configuration$Movement$Root$LinkClicked = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$Movement$FreeOnDrag$ClearAffected = {$: 1};
var author$project$Configuration$Movement$FreeOnDrag$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'grid'),
		A2(elm$html$Html$Attributes$style, 'grid-template-columns', '50px 50px 50px'),
		A2(elm$html$Html$Attributes$style, 'grid-template-rows', '50px 50px 50px'),
		A2(elm$html$Html$Attributes$style, 'grid-gap', '1em'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$Movement$FreeOnDrag$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa')
	]);
var author$project$Configuration$Movement$FreeOnDrag$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$Movement$FreeOnDrag$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$FreeOnDrag$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$FreeOnDrag$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$FreeOnDrag$draggedItemStyles,
						author$project$Configuration$Movement$FreeOnDrag$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$FreeOnDrag$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#691361')
	]);
var author$project$Configuration$Movement$FreeOnDrag$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var author$project$Configuration$Movement$FreeOnDrag$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'frdrag-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$FreeOnDrag$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$FreeOnDrag$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$FreeOnDrag$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$FreeOnDrag$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$FreeOnDrag$placeholderItemStyles),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$FreeOnDrag$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mousedown',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Configuration$Movement$FreeOnDrag$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$FreeOnDrag$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$FreeOnDrag$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$FreeOnDrag$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$FreeOnDrag$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$FreeOnDrop$ClearAffected = {$: 1};
var author$project$Configuration$Movement$FreeOnDrop$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'grid'),
		A2(elm$html$Html$Attributes$style, 'grid-template-columns', '50px 50px 50px'),
		A2(elm$html$Html$Attributes$style, 'grid-template-rows', '50px 50px 50px'),
		A2(elm$html$Html$Attributes$style, 'grid-gap', '1em'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$Movement$FreeOnDrop$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d')
	]);
var author$project$Configuration$Movement$FreeOnDrop$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$Movement$FreeOnDrop$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$FreeOnDrop$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$FreeOnDrop$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$FreeOnDrop$draggedItemStyles,
						author$project$Configuration$Movement$FreeOnDrop$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$FreeOnDrop$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#136169')
	]);
var author$project$Configuration$Movement$FreeOnDrop$overedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#63bdc7')
	]);
var author$project$Configuration$Movement$FreeOnDrop$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var author$project$Configuration$Movement$FreeOnDrop$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'frdrop-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$FreeOnDrop$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$FreeOnDrop$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$FreeOnDrop$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			var dropIndex = _n0.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$FreeOnDrop$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					_Utils_ap(
						author$project$Configuration$Movement$FreeOnDrop$overedItemStyles,
						A2(author$project$Configuration$Movement$FreeOnDrop$system.cN, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$FreeOnDrop$placeholderItemStyles),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$FreeOnDrop$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Configuration$Movement$FreeOnDrop$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$FreeOnDrop$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$FreeOnDrop$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$FreeOnDrop$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$FreeOnDrop$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$HorizontalOnDrag$ClearAffected = {$: 1};
var author$project$Configuration$Movement$HorizontalOnDrag$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex')
	]);
var author$project$Configuration$Movement$HorizontalOnDrag$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa')
	]);
var author$project$Configuration$Movement$HorizontalOnDrag$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
		A2(elm$html$Html$Attributes$style, 'width', '50px'),
		A2(elm$html$Html$Attributes$style, 'height', '50px')
	]);
var author$project$Configuration$Movement$HorizontalOnDrag$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$HorizontalOnDrag$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$HorizontalOnDrag$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$HorizontalOnDrag$draggedItemStyles,
						author$project$Configuration$Movement$HorizontalOnDrag$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrag$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#691361')
	]);
var author$project$Configuration$Movement$HorizontalOnDrag$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var author$project$Configuration$Movement$HorizontalOnDrag$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'hrdrag-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$HorizontalOnDrag$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$HorizontalOnDrag$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$HorizontalOnDrag$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$HorizontalOnDrag$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$HorizontalOnDrag$placeholderItemStyles),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$HorizontalOnDrag$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrag$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$HorizontalOnDrag$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$HorizontalOnDrag$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$HorizontalOnDrag$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$HorizontalOnDrag$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$HorizontalOnDrop$ClearAffected = {$: 1};
var author$project$Configuration$Movement$HorizontalOnDrop$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
		A2(elm$html$Html$Attributes$style, 'width', '50px'),
		A2(elm$html$Html$Attributes$style, 'height', '50px')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$HorizontalOnDrop$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$HorizontalOnDrop$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$HorizontalOnDrop$draggedItemStyles,
						author$project$Configuration$Movement$HorizontalOnDrop$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrop$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#136169')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$overedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#63bdc7')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var author$project$Configuration$Movement$HorizontalOnDrop$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'hrdrop-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$HorizontalOnDrop$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$HorizontalOnDrop$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$HorizontalOnDrop$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			var dropIndex = _n0.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$HorizontalOnDrop$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					_Utils_ap(
						author$project$Configuration$Movement$HorizontalOnDrop$overedItemStyles,
						A2(author$project$Configuration$Movement$HorizontalOnDrop$system.cN, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$HorizontalOnDrop$placeholderItemStyles),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$HorizontalOnDrop$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Configuration$Movement$HorizontalOnDrop$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$HorizontalOnDrop$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$HorizontalOnDrop$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$HorizontalOnDrop$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$HorizontalOnDrop$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$VerticalOnDrag$ClearAffected = {$: 1};
var author$project$Configuration$Movement$VerticalOnDrag$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column')
	]);
var author$project$Configuration$Movement$VerticalOnDrag$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa')
	]);
var author$project$Configuration$Movement$VerticalOnDrag$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 0 1.5em 0'),
		A2(elm$html$Html$Attributes$style, 'width', '50px'),
		A2(elm$html$Html$Attributes$style, 'height', '50px')
	]);
var author$project$Configuration$Movement$VerticalOnDrag$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$VerticalOnDrag$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$VerticalOnDrag$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$VerticalOnDrag$draggedItemStyles,
						author$project$Configuration$Movement$VerticalOnDrag$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$VerticalOnDrag$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#691361')
	]);
var author$project$Configuration$Movement$VerticalOnDrag$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var author$project$Configuration$Movement$VerticalOnDrag$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'vrdrag-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$VerticalOnDrag$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$VerticalOnDrag$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$VerticalOnDrag$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$VerticalOnDrag$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$VerticalOnDrag$placeholderItemStyles),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$VerticalOnDrag$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Configuration$Movement$VerticalOnDrag$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$VerticalOnDrag$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$VerticalOnDrag$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$VerticalOnDrag$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$VerticalOnDrag$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$VerticalOnDrop$ClearAffected = {$: 1};
var author$project$Configuration$Movement$VerticalOnDrop$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$draggedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#aa1e9d')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$itemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#1e9daa'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
		A2(elm$html$Html$Attributes$style, 'color', 'white'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'font-size', '1.2em'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 0 1.5em 0'),
		A2(elm$html$Html$Attributes$style, 'width', '50px'),
		A2(elm$html$Html$Attributes$style, 'height', '50px')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$Movement$VerticalOnDrop$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$Movement$VerticalOnDrop$itemStyles,
					_Utils_ap(
						author$project$Configuration$Movement$VerticalOnDrop$draggedItemStyles,
						author$project$Configuration$Movement$VerticalOnDrop$system.ej(draggable))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$Movement$VerticalOnDrop$affectedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#136169')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$overedItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#63bdc7')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$placeholderItemStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'dimgray')
	]);
var author$project$Configuration$Movement$VerticalOnDrop$itemView = F4(
	function (draggable, affected, index, item) {
		var itemId = 'vrdrop-' + item;
		var attrs = A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$id(itemId),
			_Utils_ap(
				author$project$Configuration$Movement$VerticalOnDrop$itemStyles,
				A2(elm$core$List$member, index, affected) ? author$project$Configuration$Movement$VerticalOnDrop$affectedItemStyles : _List_Nil));
		var _n0 = author$project$Configuration$Movement$VerticalOnDrop$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			var dropIndex = _n0.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$VerticalOnDrop$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					_Utils_ap(
						author$project$Configuration$Movement$VerticalOnDrop$overedItemStyles,
						A2(author$project$Configuration$Movement$VerticalOnDrop$system.cN, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				_Utils_ap(attrs, author$project$Configuration$Movement$VerticalOnDrop$placeholderItemStyles),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs,
					A2(author$project$Configuration$Movement$VerticalOnDrop$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Configuration$Movement$VerticalOnDrop$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$Movement$VerticalOnDrop$ClearAffected)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$Movement$VerticalOnDrop$containerStyles,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$VerticalOnDrop$itemView, model.z, model.O),
					model.A)),
				A2(author$project$Configuration$Movement$VerticalOnDrop$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$Movement$Root$demoView = function (example) {
	switch (example.$) {
		case 0:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$FreeOnDragMsg,
				author$project$Configuration$Movement$FreeOnDrag$view(mo));
		case 1:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$FreeOnDropMsg,
				author$project$Configuration$Movement$FreeOnDrop$view(mo));
		case 2:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$HorizontalOnDragMsg,
				author$project$Configuration$Movement$HorizontalOnDrag$view(mo));
		case 3:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$HorizontalOnDropMsg,
				author$project$Configuration$Movement$HorizontalOnDrop$view(mo));
		case 4:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$VerticalOnDragMsg,
				author$project$Configuration$Movement$VerticalOnDrag$view(mo));
		default:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Movement$Root$VerticalOnDropMsg,
				author$project$Configuration$Movement$VerticalOnDrop$view(mo));
	}
};
var author$project$Configuration$Movement$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {bp: 'Free on drag'};
		case 1:
			return {bp: 'Free on drop'};
		case 2:
			return {bp: 'Horizontal on drag'};
		case 3:
			return {bp: 'Horizontal on drop'};
		case 4:
			return {bp: 'Vertical on drag'};
		default:
			return {bp: 'Vertical on drop'};
	}
};
var author$project$Configuration$Movement$Root$demoWrapperView = F4(
	function (offset, currentId, id, example) {
		var title = A2(
			elm$core$Basics$composeR,
			author$project$Configuration$Movement$Root$info,
			function ($) {
				return $.bp;
			})(example);
		var globalId = offset + id;
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2(elm$html$Html$Attributes$style, 'margin', '4em 0')
				]),
			_List_fromArray(
				[
					author$project$Configuration$Movement$Root$demoView(example),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('link'),
							elm$html$Html$Events$onClick(
							author$project$Configuration$Movement$Root$LinkClicked(globalId))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(title)
						]))
				]));
	});
var author$project$Configuration$Movement$Root$view = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'flex'),
						A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
						A2(elm$html$Html$Attributes$style, 'padding-top', '2em')
					]),
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$Root$demoWrapperView, 0, model.bI),
					A2(elm$core$List$take, 2, model.aP))),
				A2(
				elm$html$Html$section,
				_List_Nil,
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$Root$demoWrapperView, 2, model.bI),
					A2(
						elm$core$List$take,
						2,
						A2(elm$core$List$drop, 2, model.aP)))),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'flex'),
						A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
					]),
				A2(
					elm$core$List$indexedMap,
					A2(author$project$Configuration$Movement$Root$demoWrapperView, 4, model.bI),
					A2(
						elm$core$List$take,
						2,
						A2(elm$core$List$drop, 4, model.aP))))
			]));
};
var author$project$Configuration$OperationOnDrag$Root$LinkClicked = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrag$InsertAfter$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$InsertAfter$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$InsertAfter$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$InsertAfter$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$InsertAfter$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$InsertAfter$itemStyles(author$project$Configuration$OperationOnDrag$InsertAfter$targetColor),
					author$project$Configuration$OperationOnDrag$InsertAfter$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$InsertAfter$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'insertafter-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$InsertAfter$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$InsertAfter$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$InsertAfter$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrag$InsertAfter$sourceColor),
					A2(author$project$Configuration$OperationOnDrag$InsertAfter$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$InsertAfter$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$InsertAfter$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$InsertAfter$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$InsertAfter$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$InsertAfter$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$InsertAfter$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$InsertAfter$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$InsertBefore$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$InsertBefore$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$InsertBefore$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$InsertBefore$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$InsertBefore$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$InsertBefore$itemStyles(author$project$Configuration$OperationOnDrag$InsertBefore$targetColor),
					author$project$Configuration$OperationOnDrag$InsertBefore$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$InsertBefore$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'insertbefore-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$InsertBefore$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$InsertBefore$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$InsertBefore$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrag$InsertBefore$sourceColor),
					A2(author$project$Configuration$OperationOnDrag$InsertBefore$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$InsertBefore$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$InsertBefore$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$InsertBefore$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$InsertBefore$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$InsertBefore$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$InsertBefore$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$InsertBefore$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$RotateIn$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$RotateIn$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$RotateIn$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$RotateIn$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$RotateIn$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$RotateIn$itemStyles(author$project$Configuration$OperationOnDrag$RotateIn$targetColor),
					author$project$Configuration$OperationOnDrag$RotateIn$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$RotateIn$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'rotatein-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$RotateIn$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$RotateIn$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$RotateIn$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrag$RotateIn$sourceColor),
					A2(author$project$Configuration$OperationOnDrag$RotateIn$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$RotateIn$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$RotateIn$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$RotateIn$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$RotateIn$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$RotateIn$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$RotateIn$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$RotateIn$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$RotateOut$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$RotateOut$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$RotateOut$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$RotateOut$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$RotateOut$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$RotateOut$itemStyles(author$project$Configuration$OperationOnDrag$RotateOut$targetColor),
					author$project$Configuration$OperationOnDrag$RotateOut$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$RotateOut$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'rotateout-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$RotateOut$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$RotateOut$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$RotateOut$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$RotateOut$sourceColor),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$RotateOut$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$RotateOut$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$RotateOut$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$RotateOut$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$RotateOut$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$RotateOut$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$Swap$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$Swap$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$Swap$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$Swap$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$Swap$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$Swap$itemStyles(author$project$Configuration$OperationOnDrag$Swap$targetColor),
					author$project$Configuration$OperationOnDrag$Swap$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$Swap$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'swap-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$Swap$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$Swap$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$Swap$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$Swap$sourceColor),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$Swap$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$Swap$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$Swap$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$Swap$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$Swap$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$Swap$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$Unmove$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrag$Unmove$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrag$Unmove$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrag$Unmove$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrag$Unmove$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrag$Unmove$itemStyles(author$project$Configuration$OperationOnDrag$Unmove$targetColor),
					author$project$Configuration$OperationOnDrag$Unmove$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrag$Unmove$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'unmove-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrag$Unmove$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrag$Unmove$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$Unmove$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrag$Unmove$sourceColor),
					A2(author$project$Configuration$OperationOnDrag$Unmove$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrag$Unmove$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrag$Unmove$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrag$Unmove$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrag$Unmove$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrag$Unmove$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrag$Unmove$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrag$Unmove$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrag$Root$demoView = function (example) {
	switch (example.$) {
		case 0:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$InsertAfterMsg,
				author$project$Configuration$OperationOnDrag$InsertAfter$view(mo));
		case 1:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$InsertBeforeMsg,
				author$project$Configuration$OperationOnDrag$InsertBefore$view(mo));
		case 2:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$RotateInMsg,
				author$project$Configuration$OperationOnDrag$RotateIn$view(mo));
		case 3:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$RotateOutMsg,
				author$project$Configuration$OperationOnDrag$RotateOut$view(mo));
		case 4:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$SwapMsg,
				author$project$Configuration$OperationOnDrag$Swap$view(mo));
		default:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrag$Root$UnmoveMsg,
				author$project$Configuration$OperationOnDrag$Unmove$view(mo));
	}
};
var author$project$Configuration$OperationOnDrag$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {bp: 'Insert after'};
		case 1:
			return {bp: 'Insert before'};
		case 2:
			return {bp: 'Rotate in'};
		case 3:
			return {bp: 'Rotate out'};
		case 4:
			return {bp: 'Swap'};
		default:
			return {bp: 'Unmove'};
	}
};
var author$project$Configuration$OperationOnDrag$Root$demoWrapperView = F3(
	function (currentId, id, example) {
		var title = A2(
			elm$core$Basics$composeR,
			author$project$Configuration$OperationOnDrag$Root$info,
			function ($) {
				return $.bp;
			})(example);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
					A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2(elm$html$Html$Attributes$style, 'margin', '4em 0')
				]),
			_List_fromArray(
				[
					author$project$Configuration$OperationOnDrag$Root$demoView(example),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('link'),
							elm$html$Html$Events$onClick(
							author$project$Configuration$OperationOnDrag$Root$LinkClicked(id))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(title)
						]))
				]));
	});
var author$project$Configuration$OperationOnDrag$Root$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		A2(
			elm$core$List$indexedMap,
			author$project$Configuration$OperationOnDrag$Root$demoWrapperView(model.bI),
			model.aP));
};
var author$project$Configuration$OperationOnDrop$Root$LinkClicked = function (a) {
	return {$: 0, a: a};
};
var author$project$Configuration$OperationOnDrop$InsertAfter$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$InsertAfter$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$InsertAfter$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$InsertAfter$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$InsertAfter$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$InsertAfter$itemStyles(author$project$Configuration$OperationOnDrop$InsertAfter$targetColor),
					author$project$Configuration$OperationOnDrop$InsertAfter$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$InsertAfter$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'insertafter-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$InsertAfter$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$InsertAfter$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$InsertAfter$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$InsertAfter$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$InsertAfter$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$InsertAfter$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$InsertAfter$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$InsertAfter$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$InsertAfter$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$InsertAfter$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$InsertAfter$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$InsertAfter$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$InsertBefore$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$InsertBefore$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$InsertBefore$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$InsertBefore$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$InsertBefore$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$InsertBefore$itemStyles(author$project$Configuration$OperationOnDrop$InsertBefore$targetColor),
					author$project$Configuration$OperationOnDrop$InsertBefore$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$InsertBefore$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'insertbefore-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$InsertBefore$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$InsertBefore$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$InsertBefore$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$InsertBefore$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$InsertBefore$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$InsertBefore$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$InsertBefore$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$InsertBefore$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$InsertBefore$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$InsertBefore$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$InsertBefore$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$InsertBefore$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$RotateIn$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$RotateIn$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$RotateIn$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$RotateIn$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$RotateIn$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$RotateIn$itemStyles(author$project$Configuration$OperationOnDrop$RotateIn$targetColor),
					author$project$Configuration$OperationOnDrop$RotateIn$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$RotateIn$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'rotatein-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$RotateIn$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$RotateIn$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$RotateIn$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$RotateIn$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$RotateIn$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$RotateIn$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$RotateIn$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$RotateIn$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$RotateIn$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$RotateIn$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$RotateIn$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$RotateIn$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$RotateOut$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$RotateOut$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$RotateOut$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$RotateOut$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$RotateOut$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$RotateOut$itemStyles(author$project$Configuration$OperationOnDrop$RotateOut$targetColor),
					author$project$Configuration$OperationOnDrop$RotateOut$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$RotateOut$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'rotateout-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$RotateOut$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$RotateOut$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$RotateOut$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$RotateOut$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$RotateOut$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$RotateOut$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$RotateOut$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$RotateOut$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$RotateOut$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$RotateOut$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$RotateOut$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$RotateOut$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$Swap$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$Swap$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$Swap$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$Swap$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$Swap$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$Swap$itemStyles(author$project$Configuration$OperationOnDrop$Swap$targetColor),
					author$project$Configuration$OperationOnDrop$Swap$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$Swap$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'swap-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$Swap$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$Swap$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$Swap$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$Swap$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$Swap$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$Swap$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$Swap$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$Swap$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$Swap$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$Swap$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$Swap$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$Swap$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$Unmove$ResetColors = {$: 1};
var author$project$Configuration$OperationOnDrop$Unmove$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Configuration$OperationOnDrop$Unmove$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px')
		]);
};
var author$project$Configuration$OperationOnDrop$Unmove$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Configuration$OperationOnDrop$Unmove$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Configuration$OperationOnDrop$Unmove$itemStyles(author$project$Configuration$OperationOnDrop$Unmove$targetColor),
					author$project$Configuration$OperationOnDrop$Unmove$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Configuration$OperationOnDrop$Unmove$itemView = F3(
	function (draggable, index, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var itemId = 'unmove-' + value;
		var attrs = function (color_) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(itemId),
				author$project$Configuration$OperationOnDrop$Unmove$itemStyles(color_));
		};
		var _n1 = author$project$Configuration$OperationOnDrop$Unmove$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, index)) && (!_Utils_eq(dropIndex, index))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$Unmove$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, index)) && _Utils_eq(dropIndex, index)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Configuration$OperationOnDrop$Unmove$sourceColor),
					A2(author$project$Configuration$OperationOnDrop$Unmove$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				attrs(author$project$Configuration$OperationOnDrop$Unmove$sourceColor),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(color),
					A2(author$project$Configuration$OperationOnDrop$Unmove$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Configuration$OperationOnDrop$Unmove$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				elm$html$Html$Events$onMouseDown(author$project$Configuration$OperationOnDrop$Unmove$ResetColors)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Configuration$OperationOnDrop$Unmove$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Configuration$OperationOnDrop$Unmove$itemView(model.z),
					model.A)),
				A2(author$project$Configuration$OperationOnDrop$Unmove$draggedItemView, model.z, model.A)
			]));
};
var author$project$Configuration$OperationOnDrop$Root$demoView = function (example) {
	switch (example.$) {
		case 0:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$InsertAfterMsg,
				author$project$Configuration$OperationOnDrop$InsertAfter$view(mo));
		case 1:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$InsertBeforeMsg,
				author$project$Configuration$OperationOnDrop$InsertBefore$view(mo));
		case 2:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$RotateInMsg,
				author$project$Configuration$OperationOnDrop$RotateIn$view(mo));
		case 3:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$RotateOutMsg,
				author$project$Configuration$OperationOnDrop$RotateOut$view(mo));
		case 4:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$SwapMsg,
				author$project$Configuration$OperationOnDrop$Swap$view(mo));
		default:
			var mo = example.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$OperationOnDrop$Root$UnmoveMsg,
				author$project$Configuration$OperationOnDrop$Unmove$view(mo));
	}
};
var author$project$Configuration$OperationOnDrop$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {bp: 'Insert after'};
		case 1:
			return {bp: 'Insert before'};
		case 2:
			return {bp: 'Rotate in'};
		case 3:
			return {bp: 'Rotate out'};
		case 4:
			return {bp: 'Swap'};
		default:
			return {bp: 'Unmove'};
	}
};
var author$project$Configuration$OperationOnDrop$Root$demoWrapperView = F3(
	function (currentId, id, example) {
		var title = A2(
			elm$core$Basics$composeR,
			author$project$Configuration$OperationOnDrop$Root$info,
			function ($) {
				return $.bp;
			})(example);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
					A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2(elm$html$Html$Attributes$style, 'margin', '4em 0')
				]),
			_List_fromArray(
				[
					author$project$Configuration$OperationOnDrop$Root$demoView(example),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('link'),
							elm$html$Html$Events$onClick(
							author$project$Configuration$OperationOnDrop$Root$LinkClicked(id))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(title)
						]))
				]));
	});
var author$project$Configuration$OperationOnDrop$Root$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		A2(
			elm$core$List$indexedMap,
			author$project$Configuration$OperationOnDrop$Root$demoWrapperView(model.bI),
			model.aP));
};
var author$project$Configuration$Root$demoView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Root$OperationOnDragMsg,
				author$project$Configuration$OperationOnDrag$Root$view(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Root$OperationOnDropMsg,
				author$project$Configuration$OperationOnDrop$Root$view(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Root$MovementMsg,
				author$project$Configuration$Movement$Root$view(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Configuration$Root$GroupsMsg,
				author$project$Configuration$Groups$Root$view(mo));
	}
};
var author$project$Configuration$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {aN: 'The behavior of the different operations applied on lists while dragging.', ae: 'operations-drag', bp: 'Operations on Drag'};
		case 1:
			return {aN: 'The behavior of the different operations applied on lists on drop.', ae: 'operations-drop', bp: 'Operations on Drop'};
		case 2:
			return {aN: 'The behavior of the Free, Horizontal and Vertical only drag movement with Swap list operation.', ae: 'movement', bp: 'Movement with Swap'};
		default:
			return {aN: 'The list state invariant is that the list has to be gathered by the grouping property, and the auxiliary items have to preserve their places.', ae: 'groups', bp: 'Groups'};
	}
};
var elm$html$Html$h2 = _VirtualDom_node('h2');
var elm$html$Html$header = _VirtualDom_node('header');
var elm$html$Html$p = _VirtualDom_node('p');
var author$project$Configuration$Root$headerView = function (model) {
	var title = A2(
		elm$core$Basics$composeR,
		author$project$Configuration$Root$info,
		function ($) {
			return $.bp;
		})(model.r);
	var description = A2(
		elm$core$Basics$composeR,
		author$project$Configuration$Root$info,
		function ($) {
			return $.aN;
		})(model.r);
	return A2(
		elm$html$Html$header,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(title)
					])),
				A2(
				elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(description)
					]))
			]));
};
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$li = _VirtualDom_node('li');
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$url$Url$Builder$toQueryPair = function (_n0) {
	var key = _n0.a;
	var value = _n0.b;
	return key + ('=' + value);
};
var elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			elm$core$String$join,
			'&',
			A2(elm$core$List$map, elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var elm$url$Url$Builder$absolute = F2(
	function (pathSegments, parameters) {
		return '/' + (A2(elm$core$String$join, '/', pathSegments) + elm$url$Url$Builder$toQuery(parameters));
	});
var author$project$Configuration$Root$linkView = function (example) {
	var path = A2(
		elm$url$Url$Builder$absolute,
		_List_fromArray(
			[
				author$project$Base$base,
				'configuration',
				A2(
				elm$core$Basics$composeR,
				author$project$Configuration$Root$info,
				function ($) {
					return $.ae;
				})(example)
			]),
		_List_Nil);
	return A2(
		elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(path)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						A2(
							elm$core$Basics$composeR,
							author$project$Configuration$Root$info,
							function ($) {
								return $.bp;
							})(example))
					]))
			]));
};
var elm$html$Html$h4 = _VirtualDom_node('h4');
var elm$html$Html$ul = _VirtualDom_node('ul');
var author$project$Configuration$Root$navigationView = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('navigation')
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$h4,
			_List_Nil,
			_List_fromArray(
				[
					elm$html$Html$text('Configuration')
				])),
			A2(
			elm$html$Html$ul,
			_List_Nil,
			A2(
				elm$core$List$map,
				author$project$Configuration$Root$linkView,
				_List_fromArray(
					[
						author$project$Configuration$Root$OperationOnDrag(author$project$Configuration$OperationOnDrag$Root$initialModel),
						author$project$Configuration$Root$OperationOnDrop(author$project$Configuration$OperationOnDrop$Root$initialModel),
						author$project$Configuration$Root$Movement(author$project$Configuration$Movement$Root$initialModel),
						author$project$Configuration$Root$Groups(author$project$Configuration$Groups$Root$initialModel)
					])))
		]));
var author$project$Gallery$Root$toCode = function (url) {
	return A2(
		author$project$CustomElement$elmCode,
		_List_fromArray(
			[
				author$project$CustomElement$href(url)
			]),
		_List_Nil);
};
var author$project$Gallery$Root$codeView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			return author$project$Gallery$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Gallery/Hanoi.elm');
		case 1:
			return author$project$Gallery$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Gallery/Puzzle.elm');
		case 2:
			return author$project$Gallery$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Gallery/Shapes.elm');
		case 3:
			return author$project$Gallery$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Gallery/TryOn.elm');
		default:
			return author$project$Gallery$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Gallery/TaskBoard.elm');
	}
};
var author$project$Gallery$Hanoi$diskStyles = F2(
	function (width, color) {
		return _List_fromArray(
			[
				A2(
				elm$html$Html$Attributes$style,
				'width',
				elm$core$String$fromInt(width) + 'px'),
				A2(elm$html$Html$Attributes$style, 'min-height', '50px'),
				A2(elm$html$Html$Attributes$style, 'background', color),
				A2(elm$html$Html$Attributes$style, 'margin-bottom', '10px')
			]);
	});
var author$project$Gallery$Hanoi$draggableDiskStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var author$project$Gallery$Hanoi$droppableDiskStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
		A2(elm$html$Html$Attributes$style, 'margin-bottom', '0'),
		A2(elm$html$Html$Attributes$style, 'height', 'auto')
	]);
var author$project$Gallery$Hanoi$maybeDraggedDisk = function (_n0) {
	var draggable = _n0.z;
	var disks = _n0.U;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, disks));
		},
		author$project$Gallery$Hanoi$system.c_(draggable));
};
var author$project$Gallery$Hanoi$paint = F3(
	function (solved, startColor, solvedColor) {
		return solved ? solvedColor : startColor;
	});
var author$project$Gallery$Hanoi$placeholderDiskStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'transparent')
	]);
var author$project$Gallery$Hanoi$diskView = F5(
	function (model, maybeTopDisk, offset, localIndex, _n0) {
		var tower = _n0.aC;
		var width = _n0.cp;
		var startColor = _n0.bV;
		var solvedColor = _n0.ck;
		var globalIndex = localIndex + offset;
		var diskId = 'id-' + elm$core$String$fromInt(globalIndex);
		var color = A3(author$project$Gallery$Hanoi$paint, model.bm, startColor, solvedColor);
		var _n1 = author$project$Gallery$Hanoi$system.c_(model.z);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			if (!localIndex) {
				var _n2 = _Utils_Tuple2(
					author$project$Gallery$Hanoi$maybeDraggedDisk(model),
					maybeTopDisk);
				if ((!_n2.a.$) && (!_n2.b.$)) {
					var draggedDisk = _n2.a.a;
					var top = _n2.b.a;
					return (_Utils_cmp(draggedDisk.cp, top.cp) < 0) ? A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(diskId),
							_Utils_ap(
								A2(author$project$Gallery$Hanoi$diskStyles, width, color),
								_Utils_ap(
									author$project$Gallery$Hanoi$droppableDiskStyles,
									A2(author$project$Gallery$Hanoi$system.cN, globalIndex, diskId)))),
						_List_Nil) : A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(diskId),
							_Utils_ap(
								A2(author$project$Gallery$Hanoi$diskStyles, width, color),
								author$project$Gallery$Hanoi$droppableDiskStyles)),
						_List_Nil);
				} else {
					return A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(diskId),
							_Utils_ap(
								A2(author$project$Gallery$Hanoi$diskStyles, width, color),
								_Utils_ap(
									author$project$Gallery$Hanoi$droppableDiskStyles,
									A2(author$project$Gallery$Hanoi$system.cN, globalIndex, diskId)))),
						_List_Nil);
				}
			} else {
				if (_Utils_eq(globalIndex, dragIndex)) {
					return A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(diskId),
							_Utils_ap(
								A2(author$project$Gallery$Hanoi$diskStyles, width, color),
								author$project$Gallery$Hanoi$placeholderDiskStyles)),
						_List_Nil);
				} else {
					return A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(diskId),
							A2(author$project$Gallery$Hanoi$diskStyles, width, color)),
						_List_Nil);
				}
			}
		} else {
			return (!localIndex) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(diskId),
					_Utils_ap(
						A2(author$project$Gallery$Hanoi$diskStyles, width, color),
						author$project$Gallery$Hanoi$droppableDiskStyles)),
				_List_Nil) : ((localIndex === 1) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(diskId),
					_Utils_ap(
						A2(author$project$Gallery$Hanoi$diskStyles, width, color),
						_Utils_ap(
							author$project$Gallery$Hanoi$draggableDiskStyles,
							A2(author$project$Gallery$Hanoi$system.ei, globalIndex, diskId)))),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(diskId),
					A2(author$project$Gallery$Hanoi$diskStyles, width, color)),
				_List_Nil));
		}
	});
var author$project$Gallery$Hanoi$draggedDiskView = function (model) {
	var _n0 = author$project$Gallery$Hanoi$maybeDraggedDisk(model);
	if (!_n0.$) {
		var width = _n0.a.cp;
		var startColor = _n0.a.bV;
		var solvedColor = _n0.a.ck;
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				A2(
					author$project$Gallery$Hanoi$diskStyles,
					width,
					A3(author$project$Gallery$Hanoi$paint, model.bm, startColor, solvedColor)),
				_Utils_ap(
					author$project$Gallery$Hanoi$draggableDiskStyles,
					author$project$Gallery$Hanoi$system.ej(model.z))),
			_List_Nil);
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Gallery$Hanoi$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-bottom', '2em')
	]);
var author$project$Gallery$Hanoi$topDisk = function (disks) {
	return elm$core$List$head(
		A2(elm$core$List$drop, 1, disks));
};
var author$project$Gallery$Hanoi$towerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'flex-end'),
		A2(elm$html$Html$Attributes$style, 'box-shadow', '0 10px 0 0 dimgray'),
		A2(elm$html$Html$Attributes$style, 'margin-right', '5em'),
		A2(elm$html$Html$Attributes$style, 'width', '300px'),
		A2(elm$html$Html$Attributes$style, 'height', '320px')
	]);
var author$project$Gallery$Hanoi$view = function (model) {
	var thirdTower = A2(
		elm$core$List$filter,
		function (_n2) {
			var tower = _n2.aC;
			return tower === 2;
		},
		model.U);
	var secondTower = A2(
		elm$core$List$filter,
		function (_n1) {
			var tower = _n1.aC;
			return tower === 1;
		},
		model.U);
	var firstTower = A2(
		elm$core$List$filter,
		function (_n0) {
			var tower = _n0.aC;
			return !tower;
		},
		model.U);
	return A2(
		elm$html$Html$section,
		author$project$Gallery$Hanoi$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Gallery$Hanoi$towerStyles,
				A2(
					elm$core$List$indexedMap,
					A3(
						author$project$Gallery$Hanoi$diskView,
						model,
						author$project$Gallery$Hanoi$topDisk(firstTower),
						0),
					firstTower)),
				A2(
				elm$html$Html$div,
				author$project$Gallery$Hanoi$towerStyles,
				A2(
					elm$core$List$indexedMap,
					A3(
						author$project$Gallery$Hanoi$diskView,
						model,
						author$project$Gallery$Hanoi$topDisk(secondTower),
						elm$core$List$length(firstTower)),
					secondTower)),
				A2(
				elm$html$Html$div,
				author$project$Gallery$Hanoi$towerStyles,
				A2(
					elm$core$List$indexedMap,
					A3(
						author$project$Gallery$Hanoi$diskView,
						model,
						author$project$Gallery$Hanoi$topDisk(thirdTower),
						elm$core$List$length(firstTower) + elm$core$List$length(secondTower)),
					thirdTower)),
				author$project$Gallery$Hanoi$draggedDiskView(model)
			]));
};
var author$project$Gallery$Puzzle$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'grid'),
		A2(elm$html$Html$Attributes$style, 'grid-template-columns', '12em 12em'),
		A2(elm$html$Html$Attributes$style, 'grid-template-rows', '12em 12em'),
		A2(elm$html$Html$Attributes$style, 'grid-gap', '2em'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding', '3em 0')
	]);
var author$project$Gallery$Puzzle$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
		]);
};
var author$project$Gallery$Puzzle$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Gallery$Puzzle$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var value = maybeDraggedItem.a.co;
			var color = maybeDraggedItem.a.b3;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Gallery$Puzzle$itemStyles(color),
					author$project$Gallery$Puzzle$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Gallery$Puzzle$groupColor = function (index) {
	switch (index) {
		case 0:
			return author$project$Gallery$Puzzle$cyan;
		case 1:
			return author$project$Gallery$Puzzle$blue;
		case 2:
			return author$project$Gallery$Puzzle$indigo;
		case 3:
			return author$project$Gallery$Puzzle$purple;
		default:
			return author$project$Gallery$Puzzle$purple;
	}
};
var author$project$Gallery$Puzzle$transparent = 'transparent';
var author$project$Gallery$Puzzle$groupStyles = F2(
	function (color, solved) {
		var bgColor = solved ? color : author$project$Gallery$Puzzle$transparent;
		return _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'background', bgColor),
				A2(elm$html$Html$Attributes$style, 'box-shadow', '0 0 0 4px ' + color),
				A2(elm$html$Html$Attributes$style, 'display', 'grid'),
				A2(elm$html$Html$Attributes$style, 'grid-template-columns', '4em 4em'),
				A2(elm$html$Html$Attributes$style, 'grid-template-rows', '4em 4em'),
				A2(elm$html$Html$Attributes$style, 'grid-gap', '1.4em'),
				A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
				A2(elm$html$Html$Attributes$style, 'padding', '1.3em')
			]);
	});
var author$project$Gallery$Puzzle$droppableStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'opacity', '0.6')
	]);
var author$project$Gallery$Puzzle$gray = 'dimgray';
var author$project$Gallery$Puzzle$itemView = F4(
	function (draggable, offset, localIndex, _n0) {
		var value = _n0.co;
		var color = _n0.b3;
		var globalIndex = localIndex + offset;
		var itemId = 'id-' + elm$core$String$fromInt(globalIndex);
		var _n1 = author$project$Gallery$Puzzle$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			var dropIndex = _n1.a.ek;
			return ((!_Utils_eq(dragIndex, globalIndex)) && (!_Utils_eq(dropIndex, globalIndex))) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Gallery$Puzzle$itemStyles(color),
						A2(author$project$Gallery$Puzzle$system.cN, globalIndex, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : (((!_Utils_eq(dragIndex, globalIndex)) && _Utils_eq(dropIndex, globalIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Gallery$Puzzle$itemStyles(color),
						_Utils_ap(
							author$project$Gallery$Puzzle$droppableStyles,
							A2(author$project$Gallery$Puzzle$system.cN, globalIndex, itemId)))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Gallery$Puzzle$itemStyles(author$project$Gallery$Puzzle$gray)),
				_List_Nil));
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Gallery$Puzzle$itemStyles(color),
						A2(author$project$Gallery$Puzzle$system.ei, globalIndex, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(value)
					]));
		}
	});
var author$project$Gallery$Puzzle$groupView = F4(
	function (draggable, quads, offset, index) {
		return A2(
			elm$html$Html$div,
			A2(
				author$project$Gallery$Puzzle$groupStyles,
				author$project$Gallery$Puzzle$groupColor(index),
				_Utils_eq(
					quads,
					A2(
						elm$core$List$take,
						4,
						A2(elm$core$List$drop, offset, author$project$Gallery$Puzzle$solution)))),
			A2(
				elm$core$List$indexedMap,
				A2(author$project$Gallery$Puzzle$itemView, draggable, offset),
				quads));
	});
var author$project$Gallery$Puzzle$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Gallery$Puzzle$containerStyles,
				A2(
					elm$core$List$map,
					function (i) {
						return A4(
							author$project$Gallery$Puzzle$groupView,
							model.z,
							A2(
								elm$core$List$take,
								4,
								A2(elm$core$List$drop, i * 4, model.A)),
							i * 4,
							i);
					},
					A2(elm$core$List$range, 0, 3))),
				A2(author$project$Gallery$Puzzle$draggedItemView, model.z, model.A)
			]));
};
var author$project$Gallery$Shapes$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$circle = elm$svg$Svg$trustedNode('circle');
var elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var author$project$Gallery$Shapes$circle = function (color) {
	return A2(
		elm$svg$Svg$circle,
		_List_fromArray(
			[
				elm$svg$Svg$Attributes$cx('50%'),
				elm$svg$Svg$Attributes$cy('50%'),
				elm$svg$Svg$Attributes$r('49'),
				elm$svg$Svg$Attributes$fill(color)
			]),
		_List_Nil);
};
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$line = elm$svg$Svg$trustedNode('line');
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var author$project$Gallery$Shapes$cross = function (color) {
	return A2(
		elm$svg$Svg$g,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$svg$Svg$line,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$x1('10'),
						elm$svg$Svg$Attributes$y1('10'),
						elm$svg$Svg$Attributes$x2('90'),
						elm$svg$Svg$Attributes$y2('90'),
						elm$svg$Svg$Attributes$strokeWidth('40'),
						elm$svg$Svg$Attributes$stroke(color)
					]),
				_List_Nil),
				A2(
				elm$svg$Svg$line,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$x1('90'),
						elm$svg$Svg$Attributes$y1('10'),
						elm$svg$Svg$Attributes$x2('10'),
						elm$svg$Svg$Attributes$y2('90'),
						elm$svg$Svg$Attributes$strokeWidth('40'),
						elm$svg$Svg$Attributes$stroke(color)
					]),
				_List_Nil)
			]));
};
var elm$svg$Svg$rect = elm$svg$Svg$trustedNode('rect');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var author$project$Gallery$Shapes$rectangle = function (color) {
	return A2(
		elm$svg$Svg$rect,
		_List_fromArray(
			[
				elm$svg$Svg$Attributes$width('100'),
				elm$svg$Svg$Attributes$height('100'),
				elm$svg$Svg$Attributes$fill(color)
			]),
		_List_Nil);
};
var elm$svg$Svg$polygon = elm$svg$Svg$trustedNode('polygon');
var elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var author$project$Gallery$Shapes$triangle = function (color) {
	return A2(
		elm$svg$Svg$polygon,
		_List_fromArray(
			[
				elm$svg$Svg$Attributes$points('50,0 100,100 0,100'),
				elm$svg$Svg$Attributes$fill(color)
			]),
		_List_Nil);
};
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var author$project$Gallery$Shapes$shapeView = F3(
	function (shape, color, dnd) {
		return A2(
			elm$svg$Svg$svg,
			_Utils_ap(
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$width('100'),
						elm$svg$Svg$Attributes$height('100'),
						elm$svg$Svg$Attributes$viewBox('0 0 100 100')
					]),
				dnd),
			_List_fromArray(
				[
					function () {
					switch (shape) {
						case 0:
							return author$project$Gallery$Shapes$circle(color);
						case 1:
							return author$project$Gallery$Shapes$cross(color);
						case 2:
							return author$project$Gallery$Shapes$rectangle(color);
						default:
							return author$project$Gallery$Shapes$triangle(color);
					}
				}()
				]));
	});
var author$project$Gallery$Shapes$wrapperStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'width', '160px'),
		A2(elm$html$Html$Attributes$style, 'height', '160px'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Gallery$Shapes$draggableItemView = F3(
	function (draggable, index, _n0) {
		var shape = _n0.a1;
		var color = _n0.b3;
		var solved = _n0.bm;
		var itemId = 'shape-' + elm$core$String$fromInt(index);
		var _n1 = author$project$Gallery$Shapes$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$wrapperStyles,
				_List_fromArray(
					[
						A3(
						author$project$Gallery$Shapes$shapeView,
						shape,
						color,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id(itemId)
							]))
					])) : A2(elm$html$Html$div, author$project$Gallery$Shapes$wrapperStyles, _List_Nil);
		} else {
			return solved ? A2(elm$html$Html$div, author$project$Gallery$Shapes$wrapperStyles, _List_Nil) : A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$wrapperStyles,
				_List_fromArray(
					[
						A3(
						author$project$Gallery$Shapes$shapeView,
						shape,
						color,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(itemId),
							A2(author$project$Gallery$Shapes$system.ei, index, itemId)))
					]));
		}
	});
var author$project$Gallery$Shapes$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Gallery$Shapes$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var shape = maybeDraggedItem.a.a1;
			var color = maybeDraggedItem.a.b3;
			return A3(
				author$project$Gallery$Shapes$shapeView,
				shape,
				color,
				author$project$Gallery$Shapes$system.ej(draggable));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Gallery$Shapes$shapeNumber = 4;
var author$project$Gallery$Shapes$droppableItemView = F3(
	function (draggable, index, _n0) {
		var shape = _n0.a1;
		var color = _n0.b3;
		var shiftedIndex = index + author$project$Gallery$Shapes$shapeNumber;
		var itemId = 'shape-' + elm$core$String$fromInt(shiftedIndex);
		var _n1 = author$project$Gallery$Shapes$system.c_(draggable);
		if (!_n1.$) {
			return A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$wrapperStyles,
				_List_fromArray(
					[
						A3(
						author$project$Gallery$Shapes$shapeView,
						shape,
						color,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(itemId),
							A2(author$project$Gallery$Shapes$system.cN, shiftedIndex, itemId)))
					]));
		} else {
			return A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$wrapperStyles,
				_List_fromArray(
					[
						A3(
						author$project$Gallery$Shapes$shapeView,
						shape,
						color,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id(itemId)
							]))
					]));
		}
	});
var author$project$Gallery$Shapes$scoreView = function (items) {
	var attempts = elm$core$String$fromInt(
		A3(
			elm$core$List$foldl,
			A2(
				elm$core$Basics$composeR,
				function ($) {
					return $.aF;
				},
				elm$core$Basics$add),
			0,
			A2(elm$core$List$drop, author$project$Gallery$Shapes$shapeNumber, items)));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'padding-bottom', '3em'),
				A2(elm$html$Html$Attributes$style, 'text-align', 'center')
			]),
		_List_fromArray(
			[
				elm$html$Html$text('Attempts: ' + attempts)
			]));
};
var author$project$Gallery$Shapes$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				author$project$Gallery$Shapes$scoreView(model.A),
				A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Gallery$Shapes$draggableItemView(model.z),
					A2(elm$core$List$take, author$project$Gallery$Shapes$shapeNumber, model.A))),
				A2(
				elm$html$Html$div,
				author$project$Gallery$Shapes$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Gallery$Shapes$droppableItemView(model.z),
					A2(elm$core$List$drop, author$project$Gallery$Shapes$shapeNumber, model.A))),
				A2(author$project$Gallery$Shapes$draggedItemView, model.z, model.A)
			]));
};
var author$project$Gallery$TaskBoard$boardStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 auto'),
		A2(elm$html$Html$Attributes$style, 'min-height', '600px'),
		A2(elm$html$Html$Attributes$style, 'padding', '3em')
	]);
var author$project$Gallery$TaskBoard$columnHeadingStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'height', '60px'),
			A2(elm$html$Html$Attributes$style, 'margin', '0'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
		]);
};
var author$project$Gallery$TaskBoard$columnStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'box-shadow', '0 0 0 1px black'),
			A2(elm$html$Html$Attributes$style, 'width', '220px')
		]);
};
var author$project$Gallery$TaskBoard$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#999999'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'height', 'calc(100% - 80px)'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '20px')
	]);
var author$project$Gallery$TaskBoard$auxiliaryCardStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', 'transparent'),
		A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
		A2(elm$html$Html$Attributes$style, 'width', '220px'),
		A2(elm$html$Html$Attributes$style, 'height', 'auto'),
		A2(elm$html$Html$Attributes$style, 'min-height', '60px')
	]);
var author$project$Gallery$TaskBoard$cardStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'background', color),
			A2(elm$html$Html$Attributes$style, 'color', 'black'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin-bottom', '20px'),
			A2(elm$html$Html$Attributes$style, 'width', '170px'),
			A2(elm$html$Html$Attributes$style, 'height', '60px')
		]);
};
var author$project$Gallery$TaskBoard$draggableCardStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var author$project$Gallery$TaskBoard$gray = '#a5a5a5';
var author$project$Gallery$TaskBoard$maybeDraggedCard = function (_n0) {
	var cardDraggable = _n0.G;
	var cards = _n0.H;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, cards));
		},
		author$project$Gallery$TaskBoard$cardSystem.c_(cardDraggable));
};
var author$project$Gallery$TaskBoard$yellow = '#ffdf76';
var author$project$Gallery$TaskBoard$eventfulCardView = F4(
	function (model, offset, localIndex, _n0) {
		var activity = _n0.v;
		var description = _n0.aN;
		var globalIndex = localIndex + offset;
		var cardId = 'card-' + elm$core$String$fromInt(globalIndex);
		var attrs = function (styles) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(cardId),
				styles);
		};
		var _n1 = _Utils_Tuple2(
			author$project$Gallery$TaskBoard$cardSystem.c_(model.G),
			author$project$Gallery$TaskBoard$maybeDraggedCard(model));
		if ((!_n1.a.$) && (!_n1.b.$)) {
			var dragIndex = _n1.a.a.cM;
			var dragCard = _n1.b.a;
			return ((description === '') && (!_Utils_eq(activity, dragCard.v))) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(author$project$Gallery$TaskBoard$auxiliaryCardStyles),
					A2(author$project$Gallery$TaskBoard$cardSystem.cN, globalIndex, cardId)),
				_List_Nil) : (((description === '') && _Utils_eq(activity, dragCard.v)) ? A2(
				elm$html$Html$div,
				attrs(author$project$Gallery$TaskBoard$auxiliaryCardStyles),
				_List_Nil) : ((!_Utils_eq(globalIndex, dragIndex)) ? A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(
						author$project$Gallery$TaskBoard$cardStyles(author$project$Gallery$TaskBoard$yellow)),
					A2(author$project$Gallery$TaskBoard$cardSystem.cN, globalIndex, cardId)),
				_List_fromArray(
					[
						elm$html$Html$text(description)
					])) : A2(
				elm$html$Html$div,
				attrs(
					author$project$Gallery$TaskBoard$cardStyles(author$project$Gallery$TaskBoard$gray)),
				_List_Nil)));
		} else {
			return (description === '') ? A2(
				elm$html$Html$div,
				attrs(author$project$Gallery$TaskBoard$auxiliaryCardStyles),
				_List_Nil) : A2(
				elm$html$Html$div,
				_Utils_ap(
					attrs(
						author$project$Gallery$TaskBoard$cardStyles(author$project$Gallery$TaskBoard$yellow)),
					_Utils_ap(
						author$project$Gallery$TaskBoard$draggableCardStyles,
						A2(author$project$Gallery$TaskBoard$cardSystem.ei, globalIndex, cardId))),
				_List_fromArray(
					[
						elm$html$Html$text(description)
					]));
		}
	});
var author$project$Gallery$TaskBoard$eventlessCardView = function (_n0) {
	var description = _n0.aN;
	return (description === '') ? A2(elm$html$Html$div, author$project$Gallery$TaskBoard$auxiliaryCardStyles, _List_Nil) : A2(
		elm$html$Html$div,
		author$project$Gallery$TaskBoard$cardStyles(author$project$Gallery$TaskBoard$yellow),
		_List_fromArray(
			[
				elm$html$Html$text(description)
			]));
};
var author$project$Gallery$TaskBoard$Heading = F2(
	function (title, color) {
		return {b3: color, bp: title};
	});
var author$project$Gallery$TaskBoard$blue = '#055b8f';
var author$project$Gallery$TaskBoard$green = '#5b8f05';
var author$project$Gallery$TaskBoard$red = '#8f055b';
var author$project$Gallery$TaskBoard$getActivity = function (cards) {
	if (!cards.b) {
		return A2(author$project$Gallery$TaskBoard$Heading, '', '');
	} else {
		var card = cards.a;
		var rest = cards.b;
		var _n1 = card.v;
		switch (_n1) {
			case 0:
				return A2(author$project$Gallery$TaskBoard$Heading, 'To Do', author$project$Gallery$TaskBoard$blue);
			case 1:
				return A2(author$project$Gallery$TaskBoard$Heading, 'Doing', author$project$Gallery$TaskBoard$red);
			default:
				return A2(author$project$Gallery$TaskBoard$Heading, 'Done', author$project$Gallery$TaskBoard$green);
		}
	}
};
var elm$html$Html$h3 = _VirtualDom_node('h3');
var author$project$Gallery$TaskBoard$columnView = F4(
	function (model, offset, index, cards) {
		var heading = author$project$Gallery$TaskBoard$getActivity(
			A2(elm$core$List$take, 1, cards));
		var columnId = 'column-' + elm$core$String$fromInt(index);
		var attrs = function (color) {
			return A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$id(columnId),
				author$project$Gallery$TaskBoard$columnStyles(color));
		};
		var _n0 = author$project$Gallery$TaskBoard$columnSystem.c_(model.I);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			var sourceElement = _n0.a.af;
			if (!_Utils_eq(dragIndex, index)) {
				return A2(
					elm$html$Html$div,
					_Utils_ap(
						attrs('transparent'),
						A2(author$project$Gallery$TaskBoard$columnSystem.cN, index, columnId)),
					_List_fromArray(
						[
							A2(
							elm$html$Html$h3,
							author$project$Gallery$TaskBoard$columnHeadingStyles(heading.b3),
							_List_fromArray(
								[
									elm$html$Html$text(heading.bp)
								])),
							A2(
							elm$html$Html$div,
							author$project$Gallery$TaskBoard$containerStyles,
							A2(elm$core$List$map, author$project$Gallery$TaskBoard$eventlessCardView, cards))
						]));
			} else {
				var height = elm$core$String$fromInt(
					elm$core$Basics$round(sourceElement.el.b8));
				return A2(
					elm$html$Html$div,
					_Utils_ap(
						attrs(author$project$Gallery$TaskBoard$gray),
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'height', height + 'px')
							])),
					_List_Nil);
			}
		} else {
			return A2(
				elm$html$Html$div,
				attrs('transparent'),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h3,
						_Utils_ap(
							author$project$Gallery$TaskBoard$columnHeadingStyles(heading.b3),
							A2(author$project$Gallery$TaskBoard$columnSystem.ei, index, columnId)),
						_List_fromArray(
							[
								elm$html$Html$text(heading.bp)
							])),
						A2(
						elm$html$Html$div,
						author$project$Gallery$TaskBoard$containerStyles,
						A2(
							elm$core$List$indexedMap,
							A2(author$project$Gallery$TaskBoard$eventfulCardView, model, offset),
							cards))
					]));
		}
	});
var author$project$Gallery$TaskBoard$draggedCardView = function (model) {
	var _n0 = author$project$Gallery$TaskBoard$maybeDraggedCard(model);
	if (!_n0.$) {
		var description = _n0.a.aN;
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				author$project$Gallery$TaskBoard$cardStyles(author$project$Gallery$TaskBoard$yellow),
				_Utils_ap(
					author$project$Gallery$TaskBoard$draggableCardStyles,
					author$project$Gallery$TaskBoard$cardSystem.ej(model.G))),
			_List_fromArray(
				[
					elm$html$Html$text(description)
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Gallery$TaskBoard$maybeDraggedColumn = function (_n0) {
	var columnDraggable = _n0.I;
	var cards = _n0.H;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(
					elm$core$List$drop,
					dragIndex,
					author$project$Gallery$TaskBoard$gatherByActivity(cards)));
		},
		author$project$Gallery$TaskBoard$columnSystem.c_(columnDraggable));
};
var author$project$Gallery$TaskBoard$draggedColumnView = function (model) {
	var _n0 = author$project$Gallery$TaskBoard$maybeDraggedColumn(model);
	if (!_n0.$) {
		var cards = _n0.a;
		var heading = author$project$Gallery$TaskBoard$getActivity(
			A2(elm$core$List$take, 1, cards));
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				author$project$Gallery$TaskBoard$columnStyles('transparent'),
				author$project$Gallery$TaskBoard$columnSystem.ej(model.I)),
			_List_fromArray(
				[
					A2(
					elm$html$Html$h3,
					author$project$Gallery$TaskBoard$columnHeadingStyles(heading.b3),
					_List_fromArray(
						[
							elm$html$Html$text(heading.bp)
						])),
					A2(
					elm$html$Html$div,
					author$project$Gallery$TaskBoard$containerStyles,
					A2(elm$core$List$map, author$project$Gallery$TaskBoard$eventlessCardView, cards))
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Gallery$TaskBoard$view = function (model) {
	var columns = author$project$Gallery$TaskBoard$gatherByActivity(model.H);
	var calculateOffset = function (columnIndex) {
		return A3(
			elm$core$List$foldl,
			elm$core$Basics$add,
			0,
			A2(
				elm$core$List$take,
				columnIndex,
				A2(elm$core$List$map, elm$core$List$length, columns)));
	};
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Gallery$TaskBoard$boardStyles,
				A2(
					elm$core$List$indexedMap,
					F2(
						function (i, column) {
							return A4(
								author$project$Gallery$TaskBoard$columnView,
								model,
								calculateOffset(i),
								i,
								column);
						}),
					columns)),
				author$project$Gallery$TaskBoard$draggedColumnView(model),
				author$project$Gallery$TaskBoard$draggedCardView(model)
			]));
};
var author$project$Gallery$TryOn$colorStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-right', '3rem')
	]);
var author$project$Gallery$TryOn$itemStyles = F3(
	function (width, height, color) {
		return _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
				A2(elm$html$Html$Attributes$style, 'margin', '0 3rem 3rem 0'),
				A2(elm$html$Html$Attributes$style, 'background-color', color),
				A2(
				elm$html$Html$Attributes$style,
				'width',
				elm$core$String$fromInt(width) + 'rem'),
				A2(
				elm$html$Html$Attributes$style,
				'height',
				elm$core$String$fromInt(height) + 'rem')
			]);
	});
var author$project$Gallery$TryOn$colorView = F3(
	function (model, index, item) {
		var width = item.cp * 5;
		var itemId = 'color-' + elm$core$String$fromInt(index);
		var height = item.b8 * 5;
		var _n0 = author$project$Gallery$TryOn$system.c_(model.z);
		if (!_n0.$) {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Gallery$TryOn$itemStyles, width, height, item.b3),
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
							]))),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Gallery$TryOn$itemStyles, width, height, item.b3),
						_Utils_ap(
							A2(author$project$Gallery$TryOn$system.ei, index, itemId),
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
								])))),
				_List_Nil);
		}
	});
var author$project$Gallery$TryOn$maybeDraggedItem = function (_n0) {
	var draggable = _n0.z;
	var items = _n0.A;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, items));
		},
		author$project$Gallery$TryOn$system.c_(draggable));
};
var author$project$Gallery$TryOn$draggedItemView = function (model) {
	var _n0 = _Utils_Tuple2(
		author$project$Gallery$TryOn$system.c_(model.z),
		author$project$Gallery$TryOn$maybeDraggedItem(model));
	if ((!_n0.a.$) && (!_n0.b.$)) {
		var dragIndex = _n0.a.a.cM;
		var targetElement = _n0.a.a.ay;
		var color = _n0.b.a.b3;
		var baseFontSize = 16;
		var height = elm$core$Basics$round(targetElement.el.b8 / baseFontSize);
		var width = elm$core$Basics$round(targetElement.el.cp / baseFontSize);
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				A3(author$project$Gallery$TryOn$itemStyles, width, height, color),
				_Utils_ap(
					author$project$Gallery$TryOn$system.ej(model.z),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Attributes$style,
							'width',
							elm$core$String$fromInt(width) + 'rem'),
							A2(
							elm$html$Html$Attributes$style,
							'height',
							elm$core$String$fromInt(height) + 'rem'),
							A2(elm$html$Html$Attributes$style, 'transition', 'width 0.5s, height 0.5s')
						]))),
			_List_Nil);
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Gallery$TryOn$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '3rem')
	]);
var author$project$Gallery$TryOn$sizeStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'baseline'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Gallery$TryOn$sizeView = F4(
	function (model, offset, index, item) {
		var width = item.cp * 4;
		var itemId = 'size-' + elm$core$String$fromInt(index);
		var height = item.b8 * 4;
		var globalIndex = index + offset;
		var _n0 = author$project$Gallery$TryOn$system.c_(model.z);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, globalIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Gallery$TryOn$itemStyles, width, height, item.b3),
						A2(author$project$Gallery$TryOn$system.cN, globalIndex, itemId))),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Gallery$TryOn$itemStyles, width, height, author$project$Gallery$TryOn$gray),
						A2(author$project$Gallery$TryOn$system.cN, globalIndex, itemId))),
				_List_Nil);
		} else {
			return (!_Utils_eq(item.b3, author$project$Gallery$TryOn$gray)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Gallery$TryOn$itemStyles, width, height, item.b3),
						A2(author$project$Gallery$TryOn$system.ei, globalIndex, itemId))),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A3(author$project$Gallery$TryOn$itemStyles, width, height, item.b3)),
				_List_Nil);
		}
	});
var author$project$Gallery$TryOn$view = function (model) {
	return A2(
		elm$html$Html$section,
		author$project$Gallery$TryOn$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Gallery$TryOn$colorStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Gallery$TryOn$colorView(model),
					A2(
						elm$core$List$filter,
						function (item) {
							return item.bk === 1;
						},
						model.A))),
				A2(
				elm$html$Html$div,
				author$project$Gallery$TryOn$sizeStyles,
				A2(
					elm$core$List$indexedMap,
					A2(
						author$project$Gallery$TryOn$sizeView,
						model,
						elm$core$List$length(
							A2(
								elm$core$List$filter,
								function (item) {
									return item.bk === 1;
								},
								model.A))),
					A2(
						elm$core$List$filter,
						function (item) {
							return !item.bk;
						},
						model.A))),
				author$project$Gallery$TryOn$draggedItemView(model)
			]));
};
var author$project$Gallery$Root$demoView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Gallery$Root$HanoiMsg,
				author$project$Gallery$Hanoi$view(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Gallery$Root$PuzzleMsg,
				author$project$Gallery$Puzzle$view(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Gallery$Root$ShapesMsg,
				author$project$Gallery$Shapes$view(mo));
		case 3:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Gallery$Root$TryOnMsg,
				author$project$Gallery$TryOn$view(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Gallery$Root$TaskBoardMsg,
				author$project$Gallery$TaskBoard$view(mo));
	}
};
var author$project$Gallery$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {aN: 'Plain list with auxiliary elements.', ae: 'hanoi', bp: 'Towers of Hanoi'};
		case 1:
			return {aN: 'List with groups without auxiliary elements.', ae: 'puzzle', bp: 'Puzzle'};
		case 2:
			return {aN: 'Plain list with Unmove operation and beforeUpdate.', ae: 'shapes', bp: 'Geometric shapes + SVG'};
		case 3:
			return {aN: 'Plain list with info.targetElement.', ae: 'try-on', bp: 'Try on'};
		default:
			return {aN: 'Two systems - one for the cards and one for the columns.', ae: 'taskboard', bp: 'Task board'};
	}
};
var author$project$Gallery$Root$headerView = function (model) {
	var title = A2(
		elm$core$Basics$composeR,
		author$project$Gallery$Root$info,
		function ($) {
			return $.bp;
		})(model.r);
	var description = A2(
		elm$core$Basics$composeR,
		author$project$Gallery$Root$info,
		function ($) {
			return $.aN;
		})(model.r);
	return A2(
		elm$html$Html$header,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(title)
					])),
				A2(
				elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(description)
					]))
			]));
};
var author$project$Gallery$Root$linkView = function (example) {
	var path = A2(
		elm$url$Url$Builder$absolute,
		_List_fromArray(
			[
				author$project$Base$base,
				'gallery',
				A2(
				elm$core$Basics$composeR,
				author$project$Gallery$Root$info,
				function ($) {
					return $.ae;
				})(example)
			]),
		_List_Nil);
	return A2(
		elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(path)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						A2(
							elm$core$Basics$composeR,
							author$project$Gallery$Root$info,
							function ($) {
								return $.bp;
							})(example))
					]))
			]));
};
var author$project$Gallery$Root$navigationView = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('navigation')
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$h4,
			_List_Nil,
			_List_fromArray(
				[
					elm$html$Html$text('Gallery')
				])),
			A2(
			elm$html$Html$ul,
			_List_Nil,
			A2(
				elm$core$List$map,
				author$project$Gallery$Root$linkView,
				_List_fromArray(
					[
						author$project$Gallery$Root$Hanoi(author$project$Gallery$Hanoi$initialModel),
						author$project$Gallery$Root$Puzzle(author$project$Gallery$Puzzle$initialModel),
						author$project$Gallery$Root$Shapes(author$project$Gallery$Shapes$initialModel),
						author$project$Gallery$Root$TryOn(author$project$Gallery$TryOn$initialModel),
						author$project$Gallery$Root$TaskBoard(author$project$Gallery$TaskBoard$initialModel)
					])))
		]));
var author$project$Home$view = function (model) {
	return elm$html$Html$text('');
};
var author$project$Introduction$Root$toCode = function (url) {
	return A2(
		author$project$CustomElement$elmCode,
		_List_fromArray(
			[
				author$project$CustomElement$href(url)
			]),
		_List_Nil);
};
var author$project$Introduction$Root$codeView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Basic.elm');
		case 1:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/BasicElmUI.elm');
		case 2:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Handle.elm');
		case 3:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Keyed.elm');
		case 4:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Margins.elm');
		case 5:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Masonry.elm');
		case 6:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Resize.elm');
		case 7:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Independents.elm');
		default:
			return author$project$Introduction$Root$toCode('https://raw.githubusercontent.com/annaghi/dnd-list/master/examples/src/Introduction/Groups.elm');
	}
};
var author$project$Introduction$Basic$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Basic$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				author$project$Introduction$Basic$system.ej(draggable),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Basic$itemView = F3(
	function (draggable, index, item) {
		var itemId = 'id-' + item;
		var _n0 = author$project$Introduction$Basic$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$p,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A2(author$project$Introduction$Basic$system.cN, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$p,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id(itemId)
					]),
				_List_fromArray(
					[
						elm$html$Html$text('[---------]')
					]));
		} else {
			return A2(
				elm$html$Html$p,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A2(author$project$Introduction$Basic$system.ei, index, itemId)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Introduction$Basic$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'text-align', 'center')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Basic$itemView(model.z),
					model.A)),
				A2(author$project$Introduction$Basic$draggedItemView, model.z, model.A)
			]));
};
var mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var mdgriffith$elm_ui$Element$height = mdgriffith$elm_ui$Internal$Model$Height;
var mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var mdgriffith$elm_ui$Element$shrink = mdgriffith$elm_ui$Internal$Model$Content;
var mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var mdgriffith$elm_ui$Element$width = mdgriffith$elm_ui$Internal$Model$Width;
var mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var mdgriffith$elm_ui$Internal$Model$asEl = 2;
var mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var mdgriffith$elm_ui$Internal$Model$div = mdgriffith$elm_ui$Internal$Model$Generic;
var mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Flag$none = A2(mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var mdgriffith$elm_ui$Internal$Style$classes = {dG: 'a', bZ: 'atv', dI: 'ab', dJ: 'cx', dK: 'cy', dL: 'acb', dM: 'accx', dN: 'accy', dO: 'acr', cw: 'al', cx: 'ar', dQ: 'at', b_: 'ah', b$: 'av', dS: 's', dW: 'bh', dX: 'b', dZ: 'w7', d$: 'bd', d0: 'bdt', bw: 'bn', d1: 'bs', by: 'cpe', d6: 'cp', d7: 'cpx', d8: 'cpy', S: 'c', bA: 'ctr', bB: 'cb', bC: 'ccx', T: 'ccy', be: 'cl', bD: 'cr', ec: 'ct', ed: 'cptr', ee: 'ctxt', ep: 'fcs', eq: 'fs', er: 'g', b7: 'hbh', bH: 'hc', b9: 'hf', cV: 'hfp', eu: 'hv', ex: 'ic', ez: 'fr', eB: 'iml', eC: 'it', eD: 'i', aY: 'nb', c6: 'notxt', eK: 'ol', eM: 'or', av: 'oq', eQ: 'oh', db: 'pg', dc: 'p', eR: 'ppe', eU: 'ui', D: 'r', eY: 'sb', eZ: 'sbx', e_: 'sby', e$: 'sbt', e1: 'e', e2: 'cap', e3: 'sev', e9: 'sk', y: 't', fb: 'tc', fc: 'w8', fd: 'w2', fe: 'w9', ff: 'tj', bX: 'tja', fg: 'tl', fh: 'w3', fi: 'w5', fj: 'w4', fk: 'tr', fl: 'w6', fm: 'w1', fn: 'tun', dy: 'ts', aD: 'clr', fs: 'u', cq: 'wc', dD: 'we', cr: 'wf', dE: 'wfp', cs: 'wrp'};
var mdgriffith$elm_ui$Internal$Model$columnClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.S);
var mdgriffith$elm_ui$Internal$Model$gridClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.er);
var mdgriffith$elm_ui$Internal$Model$pageClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.db);
var mdgriffith$elm_ui$Internal$Model$paragraphClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.dc);
var mdgriffith$elm_ui$Internal$Model$rowClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.D);
var mdgriffith$elm_ui$Internal$Model$singleClass = mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.e1);
var mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var elm$html$Html$s = _VirtualDom_node('s');
var elm$html$Html$u = _VirtualDom_node('u');
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var mdgriffith$elm_ui$Internal$Flag$alignBottom = mdgriffith$elm_ui$Internal$Flag$flag(41);
var mdgriffith$elm_ui$Internal$Flag$alignRight = mdgriffith$elm_ui$Internal$Flag$flag(40);
var mdgriffith$elm_ui$Internal$Flag$centerX = mdgriffith$elm_ui$Internal$Flag$flag(42);
var mdgriffith$elm_ui$Internal$Flag$centerY = mdgriffith$elm_ui$Internal$Flag$flag(43);
var mdgriffith$elm_ui$Internal$Flag$heightBetween = mdgriffith$elm_ui$Internal$Flag$flag(45);
var mdgriffith$elm_ui$Internal$Flag$heightFill = mdgriffith$elm_ui$Internal$Flag$flag(37);
var mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _n0) {
		var fieldOne = _n0.a;
		var fieldTwo = _n0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$widthBetween = mdgriffith$elm_ui$Internal$Flag$flag(44);
var mdgriffith$elm_ui$Internal$Flag$widthFill = mdgriffith$elm_ui$Internal$Flag$flag(39);
var elm$core$Set$Set_elm_builtin = elm$core$Basics$identity;
var elm$core$Set$empty = elm$core$Dict$empty;
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0;
		return A3(elm$core$Dict$insert, key, 0, dict);
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (!_n0.$) {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0;
		return A2(elm$core$Dict$member, key, dict);
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + (elm$core$String$fromInt(min) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + (elm$core$String$fromInt(max) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return elm$core$String$fromInt(
		elm$core$Basics$round(x * 255));
};
var mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'mv-' + (mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			return elm$core$Maybe$Just(
				'tfrm-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eV)) + ('-cols-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.J)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e4.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e4.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + (elm$core$String$fromInt(pos.D) + ('-' + (elm$core$String$fromInt(pos.d9) + ('-' + (elm$core$String$fromInt(pos.cp) + ('-' + elm$core$String$fromInt(pos.b8)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				elm$core$String$join,
				' ',
				A2(
					elm$core$List$map,
					function (sty) {
						var _n1 = mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_n1 === '') {
							return '';
						} else {
							var styleName = _n1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				elm$core$Maybe$withDefault,
				'',
				mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2(elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2(elm$core$Set$insert, styleName, cache),
			A2(elm$core$List$cons, style, existing));
	});
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var elm$core$Tuple$mapSecond = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$core$String$fromFloat = _String_fromNumber;
var mdgriffith$elm_ui$Internal$Model$formatColor = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return 'rgba(' + (elm$core$String$fromInt(
		elm$core$Basics$round(red * 255)) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(green * 255))) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(blue * 255))) + (',' + (elm$core$String$fromFloat(alpha) + ')')))));
};
var mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		elm$core$String$join,
		' ',
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.c0 ? elm$core$Maybe$Just('inset') : elm$core$Maybe$Nothing,
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.c7.a) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.c7.b) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.aJ) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.a3) + 'px'),
					elm$core$Maybe$Just(
					mdgriffith$elm_ui$Internal$Model$formatColor(shadow.b3))
				])));
};
var mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$Style,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + (':focus .focusable, ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + '.focusable:focus')),
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'border-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.d_),
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'background-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.dU),
					A2(
					elm$core$Maybe$map,
					function (shadow) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'box-shadow',
							mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
								{
									aJ: shadow.aJ,
									b3: shadow.b3,
									c0: false,
									c7: A2(
										elm$core$Tuple$mapSecond,
										elm$core$Basics$toFloat,
										A2(elm$core$Tuple$mapFirst, elm$core$Basics$toFloat, shadow.c7)),
									a3: shadow.a3
								}));
					},
					focus.e0),
					elm$core$Maybe$Just(
					A2(mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
				])));
};
var mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Left = 3;
var mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Right = 2;
var mdgriffith$elm_ui$Internal$Style$Self = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Content = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var mdgriffith$elm_ui$Internal$Style$Top = 0;
var mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _n1 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ec);
		case 1:
			var _n2 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bB);
		case 2:
			var _n3 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bD);
		case 3:
			var _n4 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.be);
		case 4:
			var _n5 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bC);
		default:
			var _n6 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.T);
	}
};
var mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _n1 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dQ);
		case 1:
			var _n2 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dI);
		case 2:
			var _n3 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cx);
		case 3:
			var _n4 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cw);
		case 4:
			var _n5 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dJ);
		default:
			var _n6 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dK);
	}
};
var mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _n0 = values(alignment);
		var content = _n0.a;
		var indiv = _n0.b;
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b7),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dW),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e$),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.y),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bH),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cq),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$Above = 0;
var mdgriffith$elm_ui$Internal$Style$Behind = 5;
var mdgriffith$elm_ui$Internal$Style$Below = 1;
var mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var mdgriffith$elm_ui$Internal$Style$Within = 4;
var mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _n0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
			_Utils_ap(
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e1),
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ex))),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + ':focus',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eU),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ez),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aY),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aY),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e1),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2(elm$core$List$map, fn, mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dG),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dX),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eM),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eK),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ez),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dW),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cs),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c6),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ed),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ee),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eR),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.by),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aD),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.av),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.eu, mdgriffith$elm_ui$Internal$Style$classes.aD)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.eu, mdgriffith$elm_ui$Internal$Style$classes.av)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.ep, mdgriffith$elm_ui$Internal$Style$classes.aD)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.ep, mdgriffith$elm_ui$Internal$Style$classes.av)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.bZ, mdgriffith$elm_ui$Internal$Style$classes.aD)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.bZ, mdgriffith$elm_ui$Internal$Style$classes.av)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dy),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							elm$core$String$join,
							', ',
							A2(
								elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eY),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eZ),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.D),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e_),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e1),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d6),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d7),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d8),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cq),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bw),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d$),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d0),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d1),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.y),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eC),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e1),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.D),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dD),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cV),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bA),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dO,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dM,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dJ),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dM,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dJ),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dM,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dK),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.dM + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.dO + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.dM)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e3),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b9),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dE),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cq),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dL,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dN,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dK),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dN,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dK),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.dN,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dK),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.dN + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.dL + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.dN)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bA),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e3),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.er),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.db),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS + ':first-child'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.dS + (mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.dS))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.dS + (mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.dS))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eB),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dc),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b7),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dW),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.y),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e1),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ez),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dW),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dG),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dX),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eM),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eK),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.y),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.D),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.er),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fm),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fd),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fh),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fj),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fi),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fl),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dZ),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fc),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fe),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eD),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e9),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fs),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fs),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e9)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fn),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ff),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bX),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fb),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fk),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fg),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var mdgriffith$elm_ui$Internal$Style$commonValues = elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			elm$core$List$map,
			function (x) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 6)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 8, 32)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + (mdgriffith$elm_ui$Internal$Style$classes.dS + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + (mdgriffith$elm_ui$Internal$Style$classes.dS + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var mdgriffith$elm_ui$Internal$Style$sliderOverrides = '\n\n/* General Input Reset */\ninput[type=range] {\n  -webkit-appearance: none; /* Hides the slider so that custom slider can be made */\n  /* width: 100%;  Specific width is required for Firefox. */\n  background: transparent; /* Otherwise white in Chrome */\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n\n/* Hide all syling for track */\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n\n/* Thumbs */\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.D) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + (' { flex-basis: auto !important; } ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.D) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dS) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bA) + (' { flex-basis: auto !important; }}' + (mdgriffith$elm_ui$Internal$Style$sliderOverrides + mdgriffith$elm_ui$Internal$Style$explainer))))))))))));
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var mdgriffith$elm_ui$Internal$Style$Intermediate = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {bz: closing, l: _List_Nil, ac: _List_Nil, N: selector};
	});
var mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_n0, rulesToRender) {
		var parent = _n0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								ac: A2(
									elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.ac)
							});
					case 2:
						var _n2 = rule.a;
						var prop = _n2.a;
						var value = _n2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								l: A2(
									elm$core$List$cons,
									{bz: '\n}', l: _List_Nil, ac: props, N: '@supports (' + (prop + (':' + (value + (') {' + parent.N))))},
									rendered.l)
							});
					case 4:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								l: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N + (' + ' + selector), ''),
										adjRules),
									rendered.l)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								l: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N + (' > ' + child), ''),
										childRules),
									rendered.l)
							});
					case 3:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								l: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.N, descriptor),
											''),
										descriptorRules),
									rendered.l)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								l: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N, ''),
										batched),
									rendered.l)
							});
				}
			});
		return A3(elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return elm$core$String$concat(
			A2(
				elm$core$List$map,
				function (_n3) {
					var x = _n3.a;
					var y = _n3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _n2 = rule.ac;
		if (!_n2.b) {
			return '';
		} else {
			return rule.N + ('{' + (renderValues(rule.ac) + (rule.bz + '}')));
		}
	};
	var renderIntermediate = function (_n0) {
		var rule = _n0;
		return _Utils_ap(
			renderClass(rule),
			elm$core$String$concat(
				A2(elm$core$List$map, renderIntermediate, rule.l)));
	};
	return elm$core$String$concat(
		A2(
			elm$core$List$map,
			renderIntermediate,
			A3(
				elm$core$List$foldr,
				F2(
					function (_n1, existing) {
						var name = _n1.a;
						var styleRules = _n1.b;
						return A2(
							elm$core$List$cons,
							A2(
								mdgriffith$elm_ui$Internal$Style$renderRules,
								A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	mdgriffith$elm_ui$Internal$Style$overrides,
	mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap(mdgriffith$elm_ui$Internal$Style$baseSheet, mdgriffith$elm_ui$Internal$Style$commonValues)));
var mdgriffith$elm_ui$Internal$Model$staticRoot = A3(
	elm$virtual_dom$VirtualDom$node,
	'style',
	_List_Nil,
	_List_fromArray(
		[
			elm$virtual_dom$VirtualDom$text(mdgriffith$elm_ui$Internal$Style$rules)
		]));
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.eJ;
			return '\"' + (name + '\"');
	}
};
var mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.dA);
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _n0, existing) {
		var key = _n0.a;
		var val = _n0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_n0) {
			var name = _n0.a;
			var val = _n0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			elm$core$String$join,
			'',
			A2(elm$core$List$map, renderPair, rules)) + '}'));
	});
var mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _n0) {
		var parentAdj = _n0.a;
		var textAdjustment = _n0.b;
		return _List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.y + (', .' + (name + (' .' + (modifier + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.y)))))))))), textAdjustment)
			]);
	});
var mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _n0, otherFontName) {
		var full = _n0.a;
		var capital = _n0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_Utils_ap(
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.e2, capital),
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.eq, full)));
	});
var mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + (', ' + ('.' + (name + (' .' + mdgriffith$elm_ui$Internal$Style$classes.e2))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.y + (', .' + (name + (' .' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.y)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$min, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {b8: height / size, a3: size, dB: vertical};
	});
var mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.d4, adjustment.dV, adjustment.eh, adjustment.eF]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		elm$core$Maybe$withDefault,
		adjustment.eh,
		elm$core$List$minimum(lines));
	var newBaseline = A2(
		elm$core$Maybe$withDefault,
		adjustment.dV,
		elm$core$List$minimum(
			A2(
				elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		elm$core$Maybe$withDefault,
		adjustment.d4,
		elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		d4: A3(mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		cT: A3(mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				elm$core$String$fromFloat(converted.b8)),
				_Utils_Tuple2(
				'vertical-align',
				elm$core$String$fromFloat(converted.dB) + 'em'),
				_Utils_Tuple2(
				'font-size',
				elm$core$String$fromFloat(converted.a3) + 'em')
			]));
};
var mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _n2 = _with.dH;
						if (_n2.$ === 1) {
							return found;
						} else {
							var adjustment = _n2.a;
							return elm$core$Maybe$Just(
								_Utils_Tuple2(
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.cT;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.d4;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		elm$core$Maybe$Nothing,
		typefaces);
};
var mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_n2) {
		var name = _n2.a;
		var typefaces = _n2.b;
		var imports = A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2(elm$core$List$map, elm$core$Tuple$first, rules);
	var fontAdjustments = function (_n1) {
		var name = _n1.a;
		var typefaces = _n1.b;
		var _n0 = mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_n0.$ === 1) {
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _n0.a;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					A2(mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontImports, rules)),
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontAdjustments, rules)));
};
var mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + elm$core$String$fromInt(index)));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return elm$core$Maybe$Just(
			A2(
				elm$core$String$join,
				', ',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$renderVariant, font.dA)));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'translate3d(' + (elm$core$String$fromFloat(x) + ('px, ' + (elm$core$String$fromFloat(y) + ('px, ' + (elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + (elm$core$String$fromFloat(tx) + ('px, ' + (elm$core$String$fromFloat(ty) + ('px, ' + (elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + (elm$core$String$fromFloat(sx) + (', ' + (elm$core$String$fromFloat(sy) + (', ' + (elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + (elm$core$String$fromFloat(ox) + (', ' + (elm$core$String$fromFloat(oy) + (', ' + (elm$core$String$fromFloat(oz) + (', ' + (elm$core$String$fromFloat(angle) + 'rad)')))))));
			return elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var renderStyle = F3(
			function (maybePseudo, selector, props) {
				if (maybePseudo.$ === 1) {
					return selector + ('{' + (A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props) + '\n}'));
				} else {
					var pseudo = maybePseudo.a;
					switch (pseudo) {
						case 1:
							var _n17 = options.eu;
							switch (_n17) {
								case 0:
									return '';
								case 2:
									return selector + ('-hv {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(true),
										'',
										props) + '\n}'));
								default:
									return selector + ('-hv:hover {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(false),
										'',
										props) + '\n}'));
							}
						case 0:
							var renderedProps = A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props);
							return A2(
								elm$core$String$join,
								'\n',
								_List_fromArray(
									[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + (mdgriffith$elm_ui$Internal$Style$classes.dS + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + (mdgriffith$elm_ui$Internal$Style$classes.dS + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), '.focusable-parent:focus ~ ' + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]));
						default:
							return selector + ('-act:active {' + (A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props) + '\n}'));
					}
				}
			});
		var renderStyleRule = F2(
			function (rule, maybePseudo) {
				switch (rule.$) {
					case 0:
						var selector = rule.a;
						var props = rule.b;
						return A3(renderStyle, maybePseudo, selector, props);
					case 13:
						var name = rule.a;
						var prop = rule.b;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
								]));
					case 12:
						var name = rule.a;
						var transparency = rule.b;
						var opacity = A2(
							elm$core$Basics$max,
							0,
							A2(elm$core$Basics$min, 1, 1 - transparency));
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'opacity',
									elm$core$String$fromFloat(opacity))
								]));
					case 2:
						var i = rule.a;
						return A3(
							renderStyle,
							maybePseudo,
							'.font-size-' + elm$core$String$fromInt(i),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'font-size',
									elm$core$String$fromInt(i) + 'px')
								]));
					case 1:
						var name = rule.a;
						var typefaces = rule.b;
						var features = A2(
							elm$core$String$join,
							', ',
							A2(elm$core$List$filterMap, mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
						var families = _List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-family',
								A2(
									elm$core$String$join,
									', ',
									A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
								A2(mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-variant',
								A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
							]);
						return A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									A3(renderStyle, maybePseudo, '.' + name, families)
								]));
					case 3:
						var _class = rule.a;
						var prop = rule.b;
						var val = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, prop, val)
								]));
					case 4:
						var _class = rule.a;
						var prop = rule.b;
						var color = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									prop,
									mdgriffith$elm_ui$Internal$Model$formatColor(color))
								]));
					case 5:
						var cls = rule.a;
						var x = rule.b;
						var y = rule.c;
						var yPx = elm$core$String$fromInt(y) + 'px';
						var xPx = elm$core$String$fromInt(x) + 'px';
						var single = '.' + mdgriffith$elm_ui$Internal$Style$classes.e1;
						var row = '.' + mdgriffith$elm_ui$Internal$Style$classes.D;
						var wrappedRow = '.' + (mdgriffith$elm_ui$Internal$Style$classes.cs + row);
						var right = '.' + mdgriffith$elm_ui$Internal$Style$classes.cx;
						var paragraph = '.' + mdgriffith$elm_ui$Internal$Style$classes.dc;
						var page = '.' + mdgriffith$elm_ui$Internal$Style$classes.db;
						var left = '.' + mdgriffith$elm_ui$Internal$Style$classes.cw;
						var halfY = elm$core$String$fromFloat(y / 2) + 'px';
						var halfX = elm$core$String$fromFloat(x / 2) + 'px';
						var column = '.' + mdgriffith$elm_ui$Internal$Style$classes.S;
						var _class = '.' + cls;
						var any = '.' + mdgriffith$elm_ui$Internal$Style$classes.dS;
						return elm$core$String$concat(
							_List_fromArray(
								[
									A3(
									renderStyle,
									maybePseudo,
									_class + (row + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (wrappedRow + (' > ' + any)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (column + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_Utils_ap(_class, paragraph),
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									'textarea' + _class,
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::after'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-top',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::before'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-bottom',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										]))
								]));
					case 7:
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'padding',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 6:
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'border-width',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 8:
						var template = rule.a;
						var toGridLengthHelper = F3(
							function (minimum, maximum, x) {
								toGridLengthHelper:
								while (true) {
									switch (x.$) {
										case 0:
											var px = x.a;
											return elm$core$String$fromInt(px) + 'px';
										case 1:
											var _n2 = _Utils_Tuple2(minimum, maximum);
											if (_n2.a.$ === 1) {
												if (_n2.b.$ === 1) {
													var _n3 = _n2.a;
													var _n4 = _n2.b;
													return 'max-content';
												} else {
													var _n6 = _n2.a;
													var maxSize = _n2.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n2.b.$ === 1) {
													var minSize = _n2.a.a;
													var _n5 = _n2.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
												} else {
													var minSize = _n2.a.a;
													var maxSize = _n2.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 2:
											var i = x.a;
											var _n7 = _Utils_Tuple2(minimum, maximum);
											if (_n7.a.$ === 1) {
												if (_n7.b.$ === 1) {
													var _n8 = _n7.a;
													var _n9 = _n7.b;
													return elm$core$String$fromInt(i) + 'fr';
												} else {
													var _n11 = _n7.a;
													var maxSize = _n7.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n7.b.$ === 1) {
													var minSize = _n7.a.a;
													var _n10 = _n7.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
												} else {
													var minSize = _n7.a.a;
													var maxSize = _n7.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 3:
											var m = x.a;
											var len = x.b;
											var $temp$minimum = elm$core$Maybe$Just(m),
												$temp$maximum = maximum,
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
										default:
											var m = x.a;
											var len = x.b;
											var $temp$minimum = minimum,
												$temp$maximum = elm$core$Maybe$Just(m),
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
									}
								}
							});
						var toGridLength = function (x) {
							return A3(toGridLengthHelper, elm$core$Maybe$Nothing, elm$core$Maybe$Nothing, x);
						};
						var xSpacing = toGridLength(template.e4.a);
						var ySpacing = toGridLength(template.e4.b);
						var rows = function (x) {
							return 'grid-template-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.eV)));
						var msRows = function (x) {
							return '-ms-grid-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.J)));
						var msColumns = function (x) {
							return '-ms-grid-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.J)));
						var gapY = 'grid-row-gap:' + (toGridLength(template.e4.b) + ';');
						var gapX = 'grid-column-gap:' + (toGridLength(template.e4.a) + ';');
						var columns = function (x) {
							return 'grid-template-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.J)));
						var _class = '.grid-rows-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eV)) + ('-cols-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.J)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e4.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e4.b)))))));
						var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msColumns + (msRows + '}')));
						return _Utils_ap(base, supports);
					case 9:
						var position = rule.a;
						var msPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'-ms-grid-row: ' + (elm$core$String$fromInt(position.D) + ';'),
									'-ms-grid-row-span: ' + (elm$core$String$fromInt(position.b8) + ';'),
									'-ms-grid-column: ' + (elm$core$String$fromInt(position.d9) + ';'),
									'-ms-grid-column-span: ' + (elm$core$String$fromInt(position.cp) + ';')
								]));
						var modernPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'grid-row: ' + (elm$core$String$fromInt(position.D) + (' / ' + (elm$core$String$fromInt(position.D + position.b8) + ';'))),
									'grid-column: ' + (elm$core$String$fromInt(position.d9) + (' / ' + (elm$core$String$fromInt(position.d9 + position.cp) + ';')))
								]));
						var _class = '.grid-pos-' + (elm$core$String$fromInt(position.D) + ('-' + (elm$core$String$fromInt(position.d9) + ('-' + (elm$core$String$fromInt(position.cp) + ('-' + elm$core$String$fromInt(position.b8)))))));
						var modernGrid = _class + ('{' + (modernPosition + '}'));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msPosition + '}'));
						return _Utils_ap(base, supports);
					case 11:
						var _class = rule.a;
						var styles = rule.b;
						var renderPseudoRule = function (style) {
							return A2(
								renderStyleRule,
								style,
								elm$core$Maybe$Just(_class));
						};
						return A2(
							elm$core$String$join,
							' ',
							A2(elm$core$List$map, renderPseudoRule, styles));
					default:
						var transform = rule.a;
						var val = mdgriffith$elm_ui$Internal$Model$transformValue(transform);
						var _class = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
						var _n12 = _Utils_Tuple2(_class, val);
						if ((!_n12.a.$) && (!_n12.b.$)) {
							var cls = _n12.a.a;
							var v = _n12.b.a;
							return A3(
								renderStyle,
								maybePseudo,
								'.' + cls,
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
									]));
						} else {
							return '';
						}
				}
			});
		var combine = F2(
			function (style, rendered) {
				return {
					bT: _Utils_ap(
						rendered.bT,
						A2(renderStyleRule, style, elm$core$Maybe$Nothing)),
					bq: function () {
						var _n14 = mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_n14.$ === 1) {
							return rendered.bq;
						} else {
							var topLevel = _n14.a;
							return A2(elm$core$List$cons, topLevel, rendered.bq);
						}
					}()
				};
			});
		var _n13 = A3(
			elm$core$List$foldl,
			combine,
			{bT: '', bq: _List_Nil},
			stylesheet);
		var topLevel = _n13.bq;
		var rules = _n13.bT;
		return _Utils_ap(
			mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			rules);
	});
var mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		return A3(
			elm$virtual_dom$VirtualDom$node,
			'style',
			_List_Nil,
			_List_fromArray(
				[
					elm$virtual_dom$VirtualDom$text(
					A2(mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
				]));
	});
var mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			_Utils_Tuple2('static-stylesheet', mdgriffith$elm_ui$Internal$Model$staticRoot),
			A2(
				elm$core$List$cons,
				_Utils_Tuple2(
					'dynamic-stylesheet',
					A2(
						mdgriffith$elm_ui$Internal$Model$toStyleSheet,
						opts,
						A3(
							elm$core$List$foldl,
							mdgriffith$elm_ui$Internal$Model$reduceStyles,
							_Utils_Tuple2(
								elm$core$Set$empty,
								_List_fromArray(
									[
										mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ep)
									])),
							styles).b)),
				children)) : A2(
			elm$core$List$cons,
			_Utils_Tuple2(
				'dynamic-stylesheet',
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ep)
								])),
						styles).b)),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			mdgriffith$elm_ui$Internal$Model$staticRoot,
			A2(
				elm$core$List$cons,
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ep)
								])),
						styles).b),
				children)) : A2(
			elm$core$List$cons,
			A2(
				mdgriffith$elm_ui$Internal$Model$toStyleSheet,
				opts,
				A3(
					elm$core$List$foldl,
					mdgriffith$elm_ui$Internal$Model$reduceStyles,
					_Utils_Tuple2(
						elm$core$Set$empty,
						_List_fromArray(
							[
								mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ep)
							])),
					styles).b),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return elm$html$Html$div;
								case 'p':
									return elm$html$Html$p;
								default:
									return elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class(mdgriffith$elm_ui$Internal$Style$classes.dS + (' ' + mdgriffith$elm_ui$Internal$Style$classes.e1))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.bA, mdgriffith$elm_ui$Internal$Style$classes.T, mdgriffith$elm_ui$Internal$Style$classes.dO])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.bA, mdgriffith$elm_ui$Internal$Style$classes.T, mdgriffith$elm_ui$Internal$Style$classes.dM])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.bA, mdgriffith$elm_ui$Internal$Style$classes.dN])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.bA, mdgriffith$elm_ui$Internal$Style$classes.dL])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.y, mdgriffith$elm_ui$Internal$Style$classes.cq, mdgriffith$elm_ui$Internal$Style$classes.bH])))
			]),
		_List_fromArray(
			[
				elm$html$Html$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A3(
		elm$virtual_dom$VirtualDom$node,
		'div',
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.y, mdgriffith$elm_ui$Internal$Style$classes.cr, mdgriffith$elm_ui$Internal$Style$classes.b9])))
			]),
		_List_fromArray(
			[
				elm$virtual_dom$VirtualDom$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_n8, _n9) {
				var key = _n8.a;
				var child = _n8.b;
				var htmls = _n9.a;
				var existingStyles = _n9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.ew, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fa : _Utils_ap(styled.fa, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.ew, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fa : _Utils_ap(styled.fa, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _n6) {
				var htmls = _n6.a;
				var existingStyles = _n6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.ew, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fa : _Utils_ap(styled.fa, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.ew, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fa : _Utils_ap(styled.fa, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _n1 = A3(
				elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _n1.a;
			var styles = _n1.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.fa : _Utils_ap(rendered.fa, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.aq,
						rendered.au,
						rendered.ak,
						mdgriffith$elm_ui$Internal$Model$Keyed(
							A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.al)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						ew: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.aq,
							rendered.au,
							rendered.ak,
							mdgriffith$elm_ui$Internal$Model$Keyed(
								A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.al))),
						fa: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _n3 = A3(
				elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _n3.a;
			var styles = _n3.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.fa : _Utils_ap(rendered.fa, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.aq,
						rendered.au,
						rendered.ak,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.al)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						ew: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.aq,
							rendered.au,
							rendered.ak,
							mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.al))),
						fa: allStyles
					});
			}
		}
	});
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$core$Bitwise$or = _Bitwise_or;
var mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _n0) {
		var one = _n0.a;
		var two = _n0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$height = mdgriffith$elm_ui$Internal$Flag$flag(7);
var mdgriffith$elm_ui$Internal$Flag$heightContent = mdgriffith$elm_ui$Internal$Flag$flag(36);
var mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_n0, _n1) {
		var one = _n0.a;
		var two = _n0.b;
		var three = _n1.a;
		var four = _n1.b;
		return A2(mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var mdgriffith$elm_ui$Internal$Flag$width = mdgriffith$elm_ui$Internal$Flag$flag(6);
var mdgriffith$elm_ui$Internal$Flag$widthContent = mdgriffith$elm_ui$Internal$Flag$flag(38);
var mdgriffith$elm_ui$Internal$Flag$xAlign = mdgriffith$elm_ui$Internal$Flag$flag(30);
var mdgriffith$elm_ui$Internal$Flag$yAlign = mdgriffith$elm_ui$Internal$Flag$flag(29);
var mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.dG]));
							case 1:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.dX]));
							case 2:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.eM]));
							case 3:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.eK]));
							case 4:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.ez]));
							default:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.aY, mdgriffith$elm_ui$Internal$Style$classes.e1, mdgriffith$elm_ui$Internal$Style$classes.dW]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html(mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.ew, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2(mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2(elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2(elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2(elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2(elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return mdgriffith$elm_ui$Internal$Style$classes.b_ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cw);
		case 2:
			return mdgriffith$elm_ui$Internal$Style$classes.b_ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cx);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.b_ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.dJ);
	}
};
var mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return mdgriffith$elm_ui$Internal$Style$classes.b$ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.dQ);
		case 2:
			return mdgriffith$elm_ui$Internal$Style$classes.b$ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.dI);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.b$ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.dK);
	}
};
var mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				name,
				_List_fromArray(
					[
						A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.bH,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.b9,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.cV + (' height-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.dS + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.D + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Style$classes.dD + (' width-px-' + elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + elm$core$String$fromInt(px),
						'width',
						elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.cq,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.cr,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.dE + (' width-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.dS + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.D + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var elm$core$Basics$ge = _Utils_ge;
var mdgriffith$elm_ui$Internal$Flag$borderWidth = mdgriffith$elm_ui$Internal$Flag$flag(27);
var mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _n1 = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_n1.$ === 1) {
					return {
						ak: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes),
							attrs),
						al: children,
						aq: has,
						au: node,
						fa: styles
					};
				} else {
					var _class = _n1.a;
					return {
						ak: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						al: children,
						aq: has,
						au: node,
						fa: A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2(elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2(mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2(elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2(mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = (mdgriffith$elm_ui$Internal$Style$classes.dD + (' width-px-' + elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(
											mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + elm$core$String$fromInt(px),
											'width',
											elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cq),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cr),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.dE + (' width-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.dS + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.D + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n4 = mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _n4.a;
									var newClass = _n4.b;
									var newStyles = _n4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = name + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.bH + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.b9 + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.cV + (' height-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.dS + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.S + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n6 = mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _n6.a;
									var newClass = _n6.b;
									var newStyles = _n6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var newNode = function () {
									switch (node.$) {
										case 0:
											return mdgriffith$elm_ui$Internal$Model$NodeName('p');
										case 1:
											var name = node.a;
											return mdgriffith$elm_ui$Internal$Model$NodeName(name);
										default:
											var x = node.a;
											var y = node.b;
											return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
									}
								}();
								var $temp$classes = classes,
									$temp$node = newNode,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.fa);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3(mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var mdgriffith$elm_ui$Internal$Model$untransformed = mdgriffith$elm_ui$Internal$Model$Untransformed;
var mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				elm$core$List$reverse(attributes)));
	});
var mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					attrs)),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Element$htmlAttribute = mdgriffith$elm_ui$Internal$Model$Attr;
var mdgriffith$elm_ui$Internal$Model$Empty = {$: 3};
var mdgriffith$elm_ui$Element$none = mdgriffith$elm_ui$Internal$Model$Empty;
var mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Element$text = function (content) {
	return mdgriffith$elm_ui$Internal$Model$Text(content);
};
var author$project$Introduction$BasicElmUI$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$BasicElmUI$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				mdgriffith$elm_ui$Element$el,
				A2(
					elm$core$List$map,
					mdgriffith$elm_ui$Element$htmlAttribute,
					author$project$Introduction$BasicElmUI$system.ej(draggable)),
				mdgriffith$elm_ui$Element$text(item));
		} else {
			return mdgriffith$elm_ui$Element$none;
		}
	});
var author$project$Introduction$BasicElmUI$itemView = F3(
	function (draggable, index, item) {
		var itemId = 'id-' + item;
		var _n0 = author$project$Introduction$BasicElmUI$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				mdgriffith$elm_ui$Element$el,
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$htmlAttribute(
						elm$html$Html$Attributes$id(itemId)),
					A2(
						elm$core$List$map,
						mdgriffith$elm_ui$Element$htmlAttribute,
						A2(author$project$Introduction$BasicElmUI$system.cN, index, itemId))),
				mdgriffith$elm_ui$Element$text(item)) : A2(
				mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$htmlAttribute(
						elm$html$Html$Attributes$id(itemId))
					]),
				mdgriffith$elm_ui$Element$text('[---------]'));
		} else {
			return A2(
				mdgriffith$elm_ui$Element$el,
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$htmlAttribute(
						elm$html$Html$Attributes$id(itemId)),
					A2(
						elm$core$List$map,
						mdgriffith$elm_ui$Element$htmlAttribute,
						A2(author$project$Introduction$BasicElmUI$system.ei, index, itemId))),
				mdgriffith$elm_ui$Element$text(item));
		}
	});
var mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var mdgriffith$elm_ui$Element$centerX = mdgriffith$elm_ui$Internal$Model$AlignX(1);
var mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_ui$Internal$Model$CenterY = 1;
var mdgriffith$elm_ui$Element$centerY = mdgriffith$elm_ui$Internal$Model$AlignY(1);
var mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return mdgriffith$elm_ui$Internal$Model$Attr(
		elm$html$Html$Attributes$class(cls));
};
var mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asColumn,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.ec + (' ' + mdgriffith$elm_ui$Internal$Style$classes.be)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Element$fill = mdgriffith$elm_ui$Internal$Model$Fill(1);
var mdgriffith$elm_ui$Internal$Model$InFront = 4;
var mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2(mdgriffith$elm_ui$Internal$Model$Nearby, 4, element);
};
var mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var mdgriffith$elm_ui$Internal$Model$Layout = 1;
var mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	dU: elm$core$Maybe$Nothing,
	d_: elm$core$Maybe$Nothing,
	e0: elm$core$Maybe$Just(
		{
			aJ: 3,
			b3: A4(mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			c7: _Utils_Tuple2(0, 0),
			a3: 3
		})
};
var mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _n4 = record.eu;
					if (_n4.$ === 1) {
						return _Utils_update(
							record,
							{
								eu: elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _n5 = record.ep;
					if (_n5.$ === 1) {
						return _Utils_update(
							record,
							{
								ep: elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _n6 = record.eH;
					if (_n6.$ === 1) {
						return _Utils_update(
							record,
							{
								eH: elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			ep: function () {
				var _n0 = record.ep;
				if (_n0.$ === 1) {
					return mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _n0.a;
					return focusable;
				}
			}(),
			eu: function () {
				var _n1 = record.eu;
				if (_n1.$ === 1) {
					return 1;
				} else {
					var hoverable = _n1.a;
					return hoverable;
				}
			}(),
			eH: function () {
				var _n2 = record.eH;
				if (_n2.$ === 1) {
					return 1;
				} else {
					var actualMode = _n2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			elm$core$List$foldr,
			combine,
			{ep: elm$core$Maybe$Nothing, eu: elm$core$Maybe$Nothing, eH: elm$core$Maybe$Nothing},
			options));
};
var mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html(mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.fa;
				var html = el.a.ew;
				return A2(
					html,
					mode(styles),
					mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _n0 = options.eH;
			if (_n0 === 2) {
				return mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asEl,
				mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var mdgriffith$elm_ui$Internal$Flag$bgColor = mdgriffith$elm_ui$Internal$Flag$flag(8);
var mdgriffith$elm_ui$Internal$Flag$fontColor = mdgriffith$elm_ui$Internal$Flag$flag(14);
var mdgriffith$elm_ui$Internal$Flag$fontFamily = mdgriffith$elm_ui$Internal$Flag$flag(5);
var mdgriffith$elm_ui$Internal$Flag$fontSize = mdgriffith$elm_ui$Internal$Flag$flag(4);
var mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var elm$core$String$toLower = _String_toLower;
var elm$core$String$words = _String_words;
var mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					default:
						var name = font.a.eJ;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
				}
			}());
	});
var mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'font-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontSize,
			mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3(elm$core$List$foldl, mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_n0, attrs, child) {
		var options = _n0.c9;
		return A3(
			mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						elm$core$String$join,
						' ',
						_List_fromArray(
							[mdgriffith$elm_ui$Internal$Style$classes.eU, mdgriffith$elm_ui$Internal$Style$classes.dS, mdgriffith$elm_ui$Internal$Style$classes.e1]))),
				_Utils_ap(mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var mdgriffith$elm_ui$Element$layout = mdgriffith$elm_ui$Element$layoutWith(
	{c9: _List_Nil});
var mdgriffith$elm_ui$Internal$Flag$padding = mdgriffith$elm_ui$Internal$Flag$flag(2);
var mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Element$padding = function (x) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + elm$core$String$fromInt(x),
			x,
			x,
			x,
			x));
};
var mdgriffith$elm_ui$Internal$Flag$spacing = mdgriffith$elm_ui$Internal$Flag$flag(3);
var mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y)));
	});
var mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2(mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var author$project$Introduction$BasicElmUI$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Element$layout,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
						mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$fill),
						mdgriffith$elm_ui$Element$inFront(
						A2(author$project$Introduction$BasicElmUI$draggedItemView, model.z, model.A))
					]),
				A2(
					mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[
							mdgriffith$elm_ui$Element$centerX,
							mdgriffith$elm_ui$Element$centerY,
							mdgriffith$elm_ui$Element$padding(10),
							mdgriffith$elm_ui$Element$spacing(10)
						]),
					A2(
						elm$core$List$indexedMap,
						author$project$Introduction$BasicElmUI$itemView(model.z),
						model.A)))
			]));
};
var author$project$Introduction$Groups$calculateOffset = F3(
	function (index, group, list) {
		calculateOffset:
		while (true) {
			if (!list.b) {
				return 0;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x.t, group)) {
					return index;
				} else {
					var $temp$index = index + 1,
						$temp$group = group,
						$temp$list = xs;
					index = $temp$index;
					group = $temp$group;
					list = $temp$list;
					continue calculateOffset;
				}
			}
		}
	});
var rtfeldman$elm_css$Css$Preprocess$AppendProperty = function (a) {
	return {$: 0, a: a};
};
var rtfeldman$elm_css$Css$property = F2(
	function (key, value) {
		return rtfeldman$elm_css$Css$Preprocess$AppendProperty(key + (':' + value));
	});
var rtfeldman$elm_css$Css$backgroundColor = function (c) {
	return A2(rtfeldman$elm_css$Css$property, 'background-color', c.co);
};
var rtfeldman$elm_css$Css$prop1 = F2(
	function (key, arg) {
		return A2(rtfeldman$elm_css$Css$property, key, arg.co);
	});
var rtfeldman$elm_css$Css$display = rtfeldman$elm_css$Css$prop1('display');
var rtfeldman$elm_css$Css$EmUnits = 0;
var rtfeldman$elm_css$Css$Structure$Compatible = 0;
var rtfeldman$elm_css$Css$Internal$lengthConverter = F3(
	function (units, unitLabel, numericValue) {
		return {
			ct: 0,
			cG: 0,
			aQ: 0,
			n: 0,
			bg: 0,
			aV: 0,
			Y: 0,
			aW: 0,
			aX: 0,
			ar: 0,
			as: 0,
			L: 0,
			aa: numericValue,
			a8: 0,
			ba: unitLabel,
			bt: units,
			co: _Utils_ap(
				elm$core$String$fromFloat(numericValue),
				unitLabel)
		};
	});
var rtfeldman$elm_css$Css$em = A2(rtfeldman$elm_css$Css$Internal$lengthConverter, 0, 'em');
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$String$cons = _String_cons;
var rtfeldman$elm_css$Css$withPrecedingHash = function (str) {
	return A2(elm$core$String$startsWith, '#', str) ? str : A2(elm$core$String$cons, '#', str);
};
var rtfeldman$elm_css$Css$erroneousHex = function (str) {
	return {
		b0: 1,
		b2: 0,
		b3: 0,
		b6: 0,
		ci: 0,
		co: rtfeldman$elm_css$Css$withPrecedingHash(str)
	};
};
var elm$core$String$fromList = _String_fromList;
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$Basics$pow = _Basics_pow;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2(elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return elm$core$Result$Err(
							elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var rtfeldman$elm_hex$Hex$fromString = function (str) {
	if (elm$core$String$isEmpty(str)) {
		return elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2(elm$core$String$startsWith, '-', str)) {
				var list = A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					elm$core$List$tail(
						elm$core$String$toList(str)));
				return A2(
					elm$core$Result$map,
					elm$core$Basics$negate,
					A3(
						rtfeldman$elm_hex$Hex$fromStringHelp,
						elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					rtfeldman$elm_hex$Hex$fromStringHelp,
					elm$core$String$length(str) - 1,
					elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2(elm$core$Result$mapError, formatError, result);
	}
};
var rtfeldman$elm_css$Css$validHex = F5(
	function (str, _n0, _n1, _n2, _n3) {
		var r1 = _n0.a;
		var r2 = _n0.b;
		var g1 = _n1.a;
		var g2 = _n1.b;
		var b1 = _n2.a;
		var b2 = _n2.b;
		var a1 = _n3.a;
		var a2 = _n3.b;
		var toResult = A2(
			elm$core$Basics$composeR,
			elm$core$String$fromList,
			A2(elm$core$Basics$composeR, elm$core$String$toLower, rtfeldman$elm_hex$Hex$fromString));
		var results = _Utils_Tuple2(
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[r1, r2])),
				toResult(
					_List_fromArray(
						[g1, g2]))),
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[b1, b2])),
				toResult(
					_List_fromArray(
						[a1, a2]))));
		if ((((!results.a.a.$) && (!results.a.b.$)) && (!results.b.a.$)) && (!results.b.b.$)) {
			var _n5 = results.a;
			var red = _n5.a.a;
			var green = _n5.b.a;
			var _n6 = results.b;
			var blue = _n6.a.a;
			var alpha = _n6.b.a;
			return {
				b0: alpha / 255,
				b2: blue,
				b3: 0,
				b6: green,
				ci: red,
				co: rtfeldman$elm_css$Css$withPrecedingHash(str)
			};
		} else {
			return rtfeldman$elm_css$Css$erroneousHex(str);
		}
	});
var rtfeldman$elm_css$Css$hex = function (str) {
	var withoutHash = A2(elm$core$String$startsWith, '#', str) ? A2(elm$core$String$dropLeft, 1, str) : str;
	var _n0 = elm$core$String$toList(withoutHash);
	_n0$4:
	while (true) {
		if ((_n0.b && _n0.b.b) && _n0.b.b.b) {
			if (!_n0.b.b.b.b) {
				var r = _n0.a;
				var _n1 = _n0.b;
				var g = _n1.a;
				var _n2 = _n1.b;
				var b = _n2.a;
				return A5(
					rtfeldman$elm_css$Css$validHex,
					str,
					_Utils_Tuple2(r, r),
					_Utils_Tuple2(g, g),
					_Utils_Tuple2(b, b),
					_Utils_Tuple2('f', 'f'));
			} else {
				if (!_n0.b.b.b.b.b) {
					var r = _n0.a;
					var _n3 = _n0.b;
					var g = _n3.a;
					var _n4 = _n3.b;
					var b = _n4.a;
					var _n5 = _n4.b;
					var a = _n5.a;
					return A5(
						rtfeldman$elm_css$Css$validHex,
						str,
						_Utils_Tuple2(r, r),
						_Utils_Tuple2(g, g),
						_Utils_Tuple2(b, b),
						_Utils_Tuple2(a, a));
				} else {
					if (_n0.b.b.b.b.b.b) {
						if (!_n0.b.b.b.b.b.b.b) {
							var r1 = _n0.a;
							var _n6 = _n0.b;
							var r2 = _n6.a;
							var _n7 = _n6.b;
							var g1 = _n7.a;
							var _n8 = _n7.b;
							var g2 = _n8.a;
							var _n9 = _n8.b;
							var b1 = _n9.a;
							var _n10 = _n9.b;
							var b2 = _n10.a;
							return A5(
								rtfeldman$elm_css$Css$validHex,
								str,
								_Utils_Tuple2(r1, r2),
								_Utils_Tuple2(g1, g2),
								_Utils_Tuple2(b1, b2),
								_Utils_Tuple2('f', 'f'));
						} else {
							if (_n0.b.b.b.b.b.b.b.b && (!_n0.b.b.b.b.b.b.b.b.b)) {
								var r1 = _n0.a;
								var _n11 = _n0.b;
								var r2 = _n11.a;
								var _n12 = _n11.b;
								var g1 = _n12.a;
								var _n13 = _n12.b;
								var g2 = _n13.a;
								var _n14 = _n13.b;
								var b1 = _n14.a;
								var _n15 = _n14.b;
								var b2 = _n15.a;
								var _n16 = _n15.b;
								var a1 = _n16.a;
								var _n17 = _n16.b;
								var a2 = _n17.a;
								return A5(
									rtfeldman$elm_css$Css$validHex,
									str,
									_Utils_Tuple2(r1, r2),
									_Utils_Tuple2(g1, g2),
									_Utils_Tuple2(b1, b2),
									_Utils_Tuple2(a1, a2));
							} else {
								break _n0$4;
							}
						}
					} else {
						break _n0$4;
					}
				}
			}
		} else {
			break _n0$4;
		}
	}
	return rtfeldman$elm_css$Css$erroneousHex(str);
};
var rtfeldman$elm_css$Css$paddingTop = rtfeldman$elm_css$Css$prop1('padding-top');
var rtfeldman$elm_css$Css$table = {f: 0, co: 'table'};
var rtfeldman$elm_css$VirtualDom$Styled$Attribute = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var Skinney$murmur3$Murmur3$HashData = F4(
	function (shift, seed, hash, charsProcessed) {
		return {aK: charsProcessed, aU: hash, ax: seed, a2: shift};
	});
var Skinney$murmur3$Murmur3$c1 = 3432918353;
var Skinney$murmur3$Murmur3$c2 = 461845907;
var Skinney$murmur3$Murmur3$multiplyBy = F2(
	function (b, a) {
		return ((a & 65535) * b) + ((((a >>> 16) * b) & 65535) << 16);
	});
var Skinney$murmur3$Murmur3$rotlBy = F2(
	function (b, a) {
		return (a << b) | (a >>> (32 - b));
	});
var Skinney$murmur3$Murmur3$finalize = function (data) {
	var acc = data.aU ? (data.ax ^ A2(
		Skinney$murmur3$Murmur3$multiplyBy,
		Skinney$murmur3$Murmur3$c2,
		A2(
			Skinney$murmur3$Murmur3$rotlBy,
			15,
			A2(Skinney$murmur3$Murmur3$multiplyBy, Skinney$murmur3$Murmur3$c1, data.aU)))) : data.ax;
	var h0 = acc ^ data.aK;
	var h1 = A2(Skinney$murmur3$Murmur3$multiplyBy, 2246822507, h0 ^ (h0 >>> 16));
	var h2 = A2(Skinney$murmur3$Murmur3$multiplyBy, 3266489909, h1 ^ (h1 >>> 13));
	return (h2 ^ (h2 >>> 16)) >>> 0;
};
var Skinney$murmur3$Murmur3$mix = F2(
	function (h1, k1) {
		return A2(
			Skinney$murmur3$Murmur3$multiplyBy,
			5,
			A2(
				Skinney$murmur3$Murmur3$rotlBy,
				13,
				h1 ^ A2(
					Skinney$murmur3$Murmur3$multiplyBy,
					Skinney$murmur3$Murmur3$c2,
					A2(
						Skinney$murmur3$Murmur3$rotlBy,
						15,
						A2(Skinney$murmur3$Murmur3$multiplyBy, Skinney$murmur3$Murmur3$c1, k1))))) + 3864292196;
	});
var Skinney$murmur3$Murmur3$hashFold = F2(
	function (c, data) {
		var res = data.aU | ((255 & elm$core$Char$toCode(c)) << data.a2);
		var _n0 = data.a2;
		if (_n0 === 24) {
			return {
				aK: data.aK + 1,
				aU: 0,
				ax: A2(Skinney$murmur3$Murmur3$mix, data.ax, res),
				a2: 0
			};
		} else {
			return {aK: data.aK + 1, aU: res, ax: data.ax, a2: data.a2 + 8};
		}
	});
var elm$core$String$foldl = _String_foldl;
var Skinney$murmur3$Murmur3$hashString = F2(
	function (seed, str) {
		return Skinney$murmur3$Murmur3$finalize(
			A3(
				elm$core$String$foldl,
				Skinney$murmur3$Murmur3$hashFold,
				A4(Skinney$murmur3$Murmur3$HashData, 0, seed, 0, 0),
				str));
	});
var elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var rtfeldman$elm_css$Css$Preprocess$stylesheet = function (snippets) {
	return {cI: elm$core$Maybe$Nothing, cY: _List_Nil, c5: _List_Nil, dq: snippets};
};
var rtfeldman$elm_css$Css$Preprocess$unwrapSnippet = function (_n0) {
	var declarations = _n0;
	return declarations;
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (!declarations.a.$) {
				var _n1 = declarations.a.a;
				var firstSelector = _n1.a;
				var otherSelectors = _n1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2(elm$core$List$cons, firstSelector, otherSelectors),
					rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$last = function (list) {
	last:
	while (true) {
		if (!list.b) {
			return elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return elm$core$Maybe$Just(singleton);
			} else {
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue last;
			}
		}
	}
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 1) {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var rtfeldman$elm_css$Css$Structure$FontFeatureValues = function (a) {
	return {$: 9, a: a};
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _List_Nil;
		} else {
			var properties = tuplesToExpand.a;
			var rest = tuplesToExpand.b;
			return A2(
				elm$core$List$cons,
				properties,
				expandTuples(rest));
		}
	};
	var newTuples = expandTuples(tuples);
	return _List_fromArray(
		[
			rtfeldman$elm_css$Css$Structure$FontFeatureValues(newTuples)
		]);
};
var rtfeldman$elm_css$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 3, a: a, b: b, c: c, d: d, e: e};
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (!declaration.$) {
			var structureStyleBlock = declaration.a;
			return A5(rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var rtfeldman$elm_css$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 0:
				var structureStyleBlock = declaration.a;
				return A2(
					rtfeldman$elm_css$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 1:
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					rtfeldman$elm_css$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 2:
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					rtfeldman$elm_css$Css$Structure$SupportsRule,
					str,
					A2(
						elm$core$List$map,
						rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 3:
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5(rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 4:
				return declaration;
			case 5:
				return declaration;
			case 6:
				return declaration;
			case 7:
				return declaration;
			case 8:
				return declaration;
			default:
				return declaration;
		}
	});
var rtfeldman$elm_css$Css$Structure$CounterStyle = function (a) {
	return {$: 8, a: a};
};
var rtfeldman$elm_css$Css$Structure$FontFace = function (a) {
	return {$: 5, a: a};
};
var rtfeldman$elm_css$Css$Structure$Keyframes = function (a) {
	return {$: 6, a: a};
};
var rtfeldman$elm_css$Css$Structure$PageRule = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var rtfeldman$elm_css$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 0, a: a};
};
var rtfeldman$elm_css$Css$Structure$Viewport = function (a) {
	return {$: 7, a: a};
};
var rtfeldman$elm_css$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(rtfeldman$elm_css$Css$Structure$mapLast, update, rest));
			}
		}
	});
var rtfeldman$elm_css$Css$Structure$withPropertyAppended = F2(
	function (property, _n0) {
		var firstSelector = _n0.a;
		var otherSelectors = _n0.b;
		var properties = _n0.c;
		return A3(
			rtfeldman$elm_css$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var rtfeldman$elm_css$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 0:
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
								A2(rtfeldman$elm_css$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 1:
						var _n1 = declarations.a;
						var mediaQueries = _n1.a;
						var styleBlocks = _n1.b;
						return _List_fromArray(
							[
								A2(
								rtfeldman$elm_css$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									rtfeldman$elm_css$Css$Structure$mapLast,
									rtfeldman$elm_css$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(rtfeldman$elm_css$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var rtfeldman$elm_css$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3(rtfeldman$elm_css$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					rtfeldman$elm_css$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2(elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3(rtfeldman$elm_css$Css$Structure$StyleBlock, first, rest, properties),
					A3(rtfeldman$elm_css$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var rtfeldman$elm_css$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _n0) {
		var sequence = _n0.a;
		var selectors = _n0.b;
		return A3(
			rtfeldman$elm_css$Css$Structure$Selector,
			sequence,
			selectors,
			elm$core$Maybe$Just(pseudo));
	});
var rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			rtfeldman$elm_css$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var rtfeldman$elm_css$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 1, a: a};
};
var rtfeldman$elm_css$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 0:
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 1:
				var list = sequence.a;
				return rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					rtfeldman$elm_css$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _n1 = list.a;
				var combinator = _n1.a;
				var sequence = _n1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2(rtfeldman$elm_css$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var rtfeldman$elm_css$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				rtfeldman$elm_css$Css$Structure$Selector,
				A2(rtfeldman$elm_css$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				rtfeldman$elm_css$Css$Structure$Selector,
				firstSelector,
				A2(rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			rtfeldman$elm_css$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_n0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 0:
							var styleBlock = declarations.a.a;
							return A2(
								elm$core$List$map,
								rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 1:
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _n1 = declarations.a;
									var mediaQueries = _n1.a;
									var _n2 = _n1.b;
									var styleBlock = _n2.a;
									return _List_fromArray(
										[
											A2(
											rtfeldman$elm_css$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _n3 = declarations.a;
									var mediaQueries = _n3.a;
									var _n4 = _n3.b;
									var first = _n4.a;
									var rest = _n4.b;
									var _n5 = A2(
										rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2(rtfeldman$elm_css$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_n5.b && (_n5.a.$ === 1)) && (!_n5.b.b)) {
										var _n6 = _n5.a;
										var newMediaQueries = _n6.a;
										var newStyleBlocks = _n6.b;
										return _List_fromArray(
											[
												A2(
												rtfeldman$elm_css$Css$Structure$MediaRule,
												newMediaQueries,
												A2(elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var newDeclarations = _n5;
										return newDeclarations;
									}
								}
							} else {
								break _n0$12;
							}
						case 2:
							var _n7 = declarations.a;
							var str = _n7.a;
							var nestedDeclarations = _n7.b;
							return _List_fromArray(
								[
									A2(
									rtfeldman$elm_css$Css$Structure$SupportsRule,
									str,
									A2(rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 3:
							var _n8 = declarations.a;
							var str1 = _n8.a;
							var str2 = _n8.b;
							var str3 = _n8.c;
							var str4 = _n8.d;
							var styleBlock = _n8.e;
							return A2(
								elm$core$List$map,
								A4(rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 4:
							var _n9 = declarations.a;
							return declarations;
						case 5:
							return declarations;
						case 6:
							return declarations;
						case 7:
							return declarations;
						case 8:
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _n0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			elm$core$List$cons,
			first,
			A2(rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule = F2(
	function (mediaQueries, declaration) {
		if (!declaration.$) {
			var styleBlock = declaration.a;
			return A2(
				rtfeldman$elm_css$Css$Structure$MediaRule,
				mediaQueries,
				_List_fromArray(
					[styleBlock]));
		} else {
			return declaration;
		}
	});
var rtfeldman$elm_css$Hash$murmurSeed = 15739;
var rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return '0';
			case 1:
				return '1';
			case 2:
				return '2';
			case 3:
				return '3';
			case 4:
				return '4';
			case 5:
				return '5';
			case 6:
				return '6';
			case 7:
				return '7';
			case 8:
				return '8';
			case 9:
				return '9';
			case 10:
				return 'a';
			case 11:
				return 'b';
			case 12:
				return 'c';
			case 13:
				return 'd';
			case 14:
				return 'e';
			case 15:
				return 'f';
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2(elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var rtfeldman$elm_hex$Hex$toString = function (num) {
	return elm$core$String$fromList(
		(num < 0) ? A2(
			elm$core$List$cons,
			'-',
			A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var rtfeldman$elm_css$Hash$fromString = function (str) {
	return A2(
		elm$core$String$cons,
		'_',
		rtfeldman$elm_hex$Hex$toString(
			A2(Skinney$murmur3$Murmur3$hashString, rtfeldman$elm_css$Hash$murmurSeed, str)));
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				elm$core$List$tail(decls));
		};
		var nextResult = A2(
			rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _n14 = _Utils_Tuple2(
				elm$core$List$head(nextResult),
				rtfeldman$elm_css$Css$Preprocess$Resolve$last(declarations));
			if ((!_n14.a.$) && (!_n14.b.$)) {
				var nextResultParent = _n14.a.a;
				var originalParent = _n14.b.a;
				return _Utils_ap(
					A2(
						elm$core$List$take,
						elm$core$List$length(declarations) - 1,
						declarations),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var insertStylesToNestedDecl = function (lastDecl) {
			return elm$core$List$concat(
				A2(
					rtfeldman$elm_css$Css$Structure$mapLast,
					rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles(nestedStyles),
					A2(
						elm$core$List$map,
						elm$core$List$singleton,
						A2(rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				elm$core$Maybe$map,
				insertStylesToNestedDecl,
				rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return _Utils_ap(
			newDeclarations,
			_Utils_ap(
				withoutParent(initialResult),
				withoutParent(nextResult)));
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return declarations;
		} else {
			switch (styles.a.$) {
				case 0:
					var property = styles.a.a;
					var rest = styles.b;
					return A2(
						rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2(rtfeldman$elm_css$Css$Structure$appendProperty, property, declarations));
				case 1:
					var _n4 = styles.a;
					var selector = _n4.a;
					var nestedStyles = _n4.b;
					var rest = styles.b;
					return A4(
						rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 2:
					var _n5 = styles.a;
					var selectorCombinator = _n5.a;
					var snippets = _n5.b;
					var rest = styles.b;
					var chain = F2(
						function (_n9, _n10) {
							var originalSequence = _n9.a;
							var originalTuples = _n9.b;
							var originalPseudoElement = _n9.c;
							var newSequence = _n10.a;
							var newTuples = _n10.b;
							var newPseudoElement = _n10.c;
							return A3(
								rtfeldman$elm_css$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 0:
								var _n7 = declaration.a;
								var firstSelector = _n7.a;
								var otherSelectors = _n7.b;
								var nestedStyles = _n7.c;
								var newSelectors = A2(
									elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											elm$core$List$map,
											chain(originalSelector),
											A2(elm$core$List$cons, firstSelector, otherSelectors));
									},
									rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remainder = newSelectors.b;
										return _List_fromArray(
											[
												rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
												A3(rtfeldman$elm_css$Css$Structure$StyleBlock, first, remainder, _List_Nil))
											]);
									}
								}();
								return A2(rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations);
							case 1:
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2(rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 2:
								var str = declaration.a;
								var otherSnippets = declaration.b;
								return A2(rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, otherSnippets);
							case 3:
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A2(
									elm$core$List$map,
									A4(rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
									rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
							case 4:
								var str = declaration.a;
								var properties = declaration.b;
								return _List_fromArray(
									[
										A2(rtfeldman$elm_css$Css$Structure$PageRule, str, properties)
									]);
							case 5:
								var properties = declaration.a;
								return _List_fromArray(
									[
										rtfeldman$elm_css$Css$Structure$FontFace(properties)
									]);
							case 6:
								var properties = declaration.a;
								return _List_fromArray(
									[
										rtfeldman$elm_css$Css$Structure$Viewport(properties)
									]);
							case 7:
								var properties = declaration.a;
								return _List_fromArray(
									[
										rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
									]);
							default:
								var tuples = declaration.a;
								return rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return elm$core$List$concat(
						_Utils_ap(
							_List_fromArray(
								[
									A2(rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								elm$core$List$map,
								expandDeclaration,
								A2(elm$core$List$concatMap, rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets))));
				case 3:
					var _n11 = styles.a;
					var pseudoElement = _n11.a;
					var nestedStyles = _n11.b;
					var rest = styles.b;
					return A4(
						rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 5:
					var str = styles.a.a;
					var rest = styles.b;
					var name = rtfeldman$elm_css$Hash$fromString(str);
					var newProperty = 'animation-name:' + name;
					var newDeclarations = A2(
						rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2(rtfeldman$elm_css$Css$Structure$appendProperty, newProperty, declarations));
					return A2(
						elm$core$List$append,
						newDeclarations,
						_List_fromArray(
							[
								rtfeldman$elm_css$Css$Structure$Keyframes(
								{ef: str, eJ: name})
							]));
				case 4:
					var _n12 = styles.a;
					var mediaQueries = _n12.a;
					var nestedStyles = _n12.b;
					var rest = styles.b;
					var extraDeclarations = function () {
						var _n13 = rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_n13.b) {
							return _List_Nil;
						} else {
							var firstSelector = _n13.a;
							var otherSelectors = _n13.b;
							return A2(
								elm$core$List$map,
								rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule(mediaQueries),
								A2(
									rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
									nestedStyles,
									elm$core$List$singleton(
										rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
											A3(rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)))));
						}
					}();
					return _Utils_ap(
						A2(rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations),
						extraDeclarations);
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock = function (_n2) {
	var firstSelector = _n2.a;
	var otherSelectors = _n2.b;
	var styles = _n2.c;
	return A2(
		rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
				A3(rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return _List_Nil;
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		return _Utils_ap(
			rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations(first),
			rtfeldman$elm_css$Css$Preprocess$Resolve$extract(rest));
	}
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			return A2(
				elm$core$List$map,
				rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
				rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		};
		return A2(elm$core$List$concatMap, handleStyleBlock, styleBlocks);
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var declarations = rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
			A2(elm$core$List$concatMap, rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
		return _List_fromArray(
			[
				A2(rtfeldman$elm_css$Css$Structure$SupportsRule, str, declarations)
			]);
	});
var rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 0:
			var styleBlock = snippetDeclaration.a;
			return rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 1:
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2(rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 2:
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2(rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 3:
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A2(
				elm$core$List$map,
				A4(rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		case 4:
			var str = snippetDeclaration.a;
			var properties = snippetDeclaration.b;
			return _List_fromArray(
				[
					A2(rtfeldman$elm_css$Css$Structure$PageRule, str, properties)
				]);
		case 5:
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					rtfeldman$elm_css$Css$Structure$FontFace(properties)
				]);
		case 6:
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					rtfeldman$elm_css$Css$Structure$Viewport(properties)
				]);
		case 7:
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
				]);
		default:
			var tuples = snippetDeclaration.a;
			return rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure = function (_n0) {
	var charset = _n0.cI;
	var imports = _n0.cY;
	var namespaces = _n0.c5;
	var snippets = _n0.dq;
	var declarations = rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
		A2(elm$core$List$concatMap, rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
	return {cI: charset, eg: declarations, cY: imports, c5: namespaces};
};
var rtfeldman$elm_css$Css$Structure$compactHelp = F2(
	function (declaration, _n0) {
		var keyframesByName = _n0.a;
		var declarations = _n0.b;
		switch (declaration.$) {
			case 0:
				var _n2 = declaration.a;
				var properties = _n2.c;
				return elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 1:
				var styleBlocks = declaration.b;
				return A2(
					elm$core$List$all,
					function (_n3) {
						var properties = _n3.c;
						return elm$core$List$isEmpty(properties);
					},
					styleBlocks) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 2:
				var otherDeclarations = declaration.b;
				return elm$core$List$isEmpty(otherDeclarations) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 3:
				return _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 4:
				var properties = declaration.b;
				return elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 5:
				var properties = declaration.a;
				return elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 6:
				var record = declaration.a;
				return elm$core$String$isEmpty(record.ef) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					A3(elm$core$Dict$insert, record.eJ, record.ef, keyframesByName),
					declarations);
			case 7:
				var properties = declaration.a;
				return elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			case 8:
				var properties = declaration.a;
				return elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
			default:
				var tuples = declaration.a;
				return A2(
					elm$core$List$all,
					function (_n4) {
						var properties = _n4.b;
						return elm$core$List$isEmpty(properties);
					},
					tuples) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2(elm$core$List$cons, declaration, declarations));
		}
	});
var rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations = F2(
	function (keyframesByName, compactedDeclarations) {
		return A2(
			elm$core$List$append,
			A2(
				elm$core$List$map,
				function (_n0) {
					var name = _n0.a;
					var decl = _n0.b;
					return rtfeldman$elm_css$Css$Structure$Keyframes(
						{ef: decl, eJ: name});
				},
				elm$core$Dict$toList(keyframesByName)),
			compactedDeclarations);
	});
var rtfeldman$elm_css$Css$Structure$compactStylesheet = function (_n0) {
	var charset = _n0.cI;
	var imports = _n0.cY;
	var namespaces = _n0.c5;
	var declarations = _n0.eg;
	var _n1 = A3(
		elm$core$List$foldr,
		rtfeldman$elm_css$Css$Structure$compactHelp,
		_Utils_Tuple2(elm$core$Dict$empty, _List_Nil),
		declarations);
	var keyframesByName = _n1.a;
	var compactedDeclarations = _n1.b;
	var finalDeclarations = A2(rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations, keyframesByName, compactedDeclarations);
	return {cI: charset, eg: finalDeclarations, cY: imports, c5: namespaces};
};
var rtfeldman$elm_css$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		elm$core$Maybe$withDefault,
		'',
		A2(
			elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.cR + (A2(
		elm$core$Maybe$withDefault,
		'',
		A2(
			elm$core$Maybe$map,
			elm$core$Basics$append(': '),
			expression.co)) + ')'));
};
var rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType) {
		case 0:
			return 'print';
		case 1:
			return 'screen';
		default:
			return 'speech';
	}
};
var rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				elm$core$String$join,
				' and ',
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString(mediaType),
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 0:
			var expressions = mediaQuery.a;
			return A2(
				elm$core$String$join,
				' and ',
				A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions));
		case 1:
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 2:
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString = F2(
	function (name, mediaQuery) {
		return '@import \"' + (name + (rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString(mediaQuery) + '\"'));
	});
var rtfeldman$elm_css$Css$Structure$Output$importToString = function (_n0) {
	var name = _n0.a;
	var mediaQueries = _n0.b;
	return A2(
		elm$core$String$join,
		'\n',
		A2(
			elm$core$List$map,
			rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString(name),
			mediaQueries));
};
var rtfeldman$elm_css$Css$Structure$Output$namespaceToString = function (_n0) {
	var prefix = _n0.a;
	var str = _n0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var rtfeldman$elm_css$Css$Structure$Output$spaceIndent = '    ';
var rtfeldman$elm_css$Css$Structure$Output$indent = function (str) {
	return _Utils_ap(rtfeldman$elm_css$Css$Structure$Output$spaceIndent, str);
};
var rtfeldman$elm_css$Css$Structure$Output$noIndent = '';
var rtfeldman$elm_css$Css$Structure$Output$emitProperty = function (str) {
	return str + ';';
};
var rtfeldman$elm_css$Css$Structure$Output$emitProperties = function (properties) {
	return A2(
		elm$core$String$join,
		'\n',
		A2(
			elm$core$List$map,
			A2(elm$core$Basics$composeL, rtfeldman$elm_css$Css$Structure$Output$indent, rtfeldman$elm_css$Css$Structure$Output$emitProperty),
			properties));
};
var elm$core$String$append = _String_append;
var rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString = function (_n0) {
	var str = _n0;
	return '::' + str;
};
var rtfeldman$elm_css$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator) {
		case 0:
			return '+';
		case 1:
			return '~';
		case 2:
			return '>';
		default:
			return '';
	}
};
var rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 0:
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 1:
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		case 2:
			var str = repeatableSimpleSelector.a;
			return ':' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return '[' + (str + ']');
	}
};
var rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 0:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$cons,
					str,
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
		case 1:
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A2(
				elm$core$String$join,
				'',
				A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors));
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$cons,
					str,
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
	}
};
var rtfeldman$elm_css$Css$Structure$Output$selectorChainToString = function (_n0) {
	var combinator = _n0.a;
	var sequence = _n0.b;
	return A2(
		elm$core$String$join,
		' ',
		_List_fromArray(
			[
				rtfeldman$elm_css$Css$Structure$Output$combinatorToString(combinator),
				rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(sequence)
			]));
};
var rtfeldman$elm_css$Css$Structure$Output$selectorToString = function (_n0) {
	var simpleSelectorSequence = _n0.a;
	var chain = _n0.b;
	var pseudoElement = _n0.c;
	var segments = A2(
		elm$core$List$cons,
		rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence),
		A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		elm$core$String$join,
		'',
		_List_fromArray(
			[
				A2(
				elm$core$Maybe$withDefault,
				'',
				A2(elm$core$Maybe$map, rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString, pseudoElement))
			]));
	return A2(
		elm$core$String$append,
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$filter,
				A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$String$isEmpty),
				segments)),
		pseudoElementsString);
};
var rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock = F2(
	function (indentLevel, _n0) {
		var firstSelector = _n0.a;
		var otherSelectors = _n0.b;
		var properties = _n0.c;
		var selectorStr = A2(
			elm$core$String$join,
			', ',
			A2(
				elm$core$List$map,
				rtfeldman$elm_css$Css$Structure$Output$selectorToString,
				A2(elm$core$List$cons, firstSelector, otherSelectors)));
		return A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					selectorStr,
					' {\n',
					indentLevel,
					rtfeldman$elm_css$Css$Structure$Output$emitProperties(properties),
					'\n',
					indentLevel,
					'}'
				]));
	});
var rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration = function (decl) {
	switch (decl.$) {
		case 0:
			var styleBlock = decl.a;
			return A2(rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock, rtfeldman$elm_css$Css$Structure$Output$noIndent, styleBlock);
		case 1:
			var mediaQueries = decl.a;
			var styleBlocks = decl.b;
			var query = A2(
				elm$core$String$join,
				',\n',
				A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString, mediaQueries));
			var blocks = A2(
				elm$core$String$join,
				'\n\n',
				A2(
					elm$core$List$map,
					A2(
						elm$core$Basics$composeL,
						rtfeldman$elm_css$Css$Structure$Output$indent,
						rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock(rtfeldman$elm_css$Css$Structure$Output$spaceIndent)),
					styleBlocks));
			return '@media ' + (query + (' {\n' + (blocks + '\n}')));
		case 2:
			return 'TODO';
		case 3:
			return 'TODO';
		case 4:
			return 'TODO';
		case 5:
			return 'TODO';
		case 6:
			var name = decl.a.eJ;
			var declaration = decl.a.ef;
			return '@keyframes ' + (name + (' {\n' + (declaration + '\n}')));
		case 7:
			return 'TODO';
		case 8:
			return 'TODO';
		default:
			return 'TODO';
	}
};
var rtfeldman$elm_css$Css$Structure$Output$prettyPrint = function (_n0) {
	var charset = _n0.cI;
	var imports = _n0.cY;
	var namespaces = _n0.c5;
	var declarations = _n0.eg;
	return A2(
		elm$core$String$join,
		'\n\n',
		A2(
			elm$core$List$filter,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$String$isEmpty),
			_List_fromArray(
				[
					rtfeldman$elm_css$Css$Structure$Output$charsetToString(charset),
					A2(
					elm$core$String$join,
					'\n',
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$importToString, imports)),
					A2(
					elm$core$String$join,
					'\n',
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$namespaceToString, namespaces)),
					A2(
					elm$core$String$join,
					'\n\n',
					A2(elm$core$List$map, rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration, declarations))
				])));
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$compileHelp = function (sheet) {
	return rtfeldman$elm_css$Css$Structure$Output$prettyPrint(
		rtfeldman$elm_css$Css$Structure$compactStylesheet(
			rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure(sheet)));
};
var rtfeldman$elm_css$Css$Preprocess$Resolve$compile = function (styles) {
	return A2(
		elm$core$String$join,
		'\n\n',
		A2(elm$core$List$map, rtfeldman$elm_css$Css$Preprocess$Resolve$compileHelp, styles));
};
var rtfeldman$elm_css$Css$Preprocess$Snippet = elm$core$Basics$identity;
var rtfeldman$elm_css$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 0, a: a};
};
var rtfeldman$elm_css$VirtualDom$Styled$makeSnippet = F2(
	function (styles, sequence) {
		var selector = A3(rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, elm$core$Maybe$Nothing);
		return _List_fromArray(
			[
				rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
				A3(rtfeldman$elm_css$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
			]);
	});
var rtfeldman$elm_css$VirtualDom$Styled$murmurSeed = 15739;
var rtfeldman$elm_css$VirtualDom$Styled$getClassname = function (styles) {
	return elm$core$List$isEmpty(styles) ? 'unstyled' : A2(
		elm$core$String$cons,
		'_',
		rtfeldman$elm_hex$Hex$toString(
			A2(
				Skinney$murmur3$Murmur3$hashString,
				rtfeldman$elm_css$VirtualDom$Styled$murmurSeed,
				rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
					elm$core$List$singleton(
						rtfeldman$elm_css$Css$Preprocess$stylesheet(
							elm$core$List$singleton(
								A2(
									rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
									styles,
									rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(_List_Nil)))))))));
};
var rtfeldman$elm_css$Html$Styled$Internal$css = function (styles) {
	var classname = rtfeldman$elm_css$VirtualDom$Styled$getClassname(styles);
	var classProperty = A2(
		elm$virtual_dom$VirtualDom$property,
		'className',
		elm$json$Json$Encode$string(classname));
	return A3(rtfeldman$elm_css$VirtualDom$Styled$Attribute, classProperty, styles, classname);
};
var rtfeldman$elm_css$Html$Styled$Attributes$css = rtfeldman$elm_css$Html$Styled$Internal$css;
var author$project$Introduction$Groups$containerStyles = function (color) {
	return rtfeldman$elm_css$Html$Styled$Attributes$css(
		_List_fromArray(
			[
				rtfeldman$elm_css$Css$display(rtfeldman$elm_css$Css$table),
				rtfeldman$elm_css$Css$backgroundColor(
				rtfeldman$elm_css$Css$hex(color)),
				rtfeldman$elm_css$Css$paddingTop(
				rtfeldman$elm_css$Css$em(3))
			]));
};
var rtfeldman$elm_css$Css$Internal$property = F2(
	function (key, value) {
		return rtfeldman$elm_css$Css$Preprocess$AppendProperty(key + (':' + value));
	});
var rtfeldman$elm_css$Css$Preprocess$ApplyStyles = function (a) {
	return {$: 6, a: a};
};
var rtfeldman$elm_css$Css$Internal$getOverloadedProperty = F3(
	function (functionName, desiredKey, style) {
		getOverloadedProperty:
		while (true) {
			switch (style.$) {
				case 0:
					var str = style.a;
					var key = A2(
						elm$core$Maybe$withDefault,
						'',
						elm$core$List$head(
							A2(elm$core$String$split, ':', str)));
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, key);
				case 1:
					var selector = style.a;
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-selector'));
				case 2:
					var combinator = style.a;
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-combinator'));
				case 3:
					var pseudoElement = style.a;
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-pseudo-element setter'));
				case 4:
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-media-query'));
				case 5:
					return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-keyframes'));
				default:
					if (!style.a.b) {
						return A2(rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-empty-Style'));
					} else {
						if (!style.a.b.b) {
							var _n1 = style.a;
							var only = _n1.a;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = only;
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						} else {
							var _n2 = style.a;
							var first = _n2.a;
							var rest = _n2.b;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = rtfeldman$elm_css$Css$Preprocess$ApplyStyles(rest);
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						}
					}
			}
		}
	});
var rtfeldman$elm_css$Css$Internal$IncompatibleUnits = 0;
var rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty = A3(rtfeldman$elm_css$Css$Internal$lengthConverter, 0, '', 0);
var rtfeldman$elm_css$Css$alignItems = function (fn) {
	return A3(
		rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'alignItems',
		'align-items',
		fn(rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var rtfeldman$elm_css$Css$auto = {dP: 0, a: 0, aQ: 0, bJ: 0, eE: 0, aV: 0, Y: 0, L: 0, aZ: 0, C: 0, bW: 0, a9: 0, u: 0, co: 'auto'};
var rtfeldman$elm_css$Css$borderRadius = rtfeldman$elm_css$Css$prop1('border-radius');
var rtfeldman$elm_css$Css$center = rtfeldman$elm_css$Css$prop1('center');
var rtfeldman$elm_css$Css$color = function (c) {
	return A2(rtfeldman$elm_css$Css$property, 'color', c.co);
};
var rtfeldman$elm_css$Css$cursor = rtfeldman$elm_css$Css$prop1('cursor');
var rtfeldman$elm_css$Css$displayFlex = A2(rtfeldman$elm_css$Css$property, 'display', 'flex');
var rtfeldman$elm_css$Css$height = rtfeldman$elm_css$Css$prop1('height');
var rtfeldman$elm_css$Css$justifyContent = function (fn) {
	return A3(
		rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'justifyContent',
		'justify-content',
		fn(rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var rtfeldman$elm_css$Css$prop4 = F5(
	function (key, argA, argB, argC, argD) {
		return A2(
			rtfeldman$elm_css$Css$property,
			key,
			A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					[argA.co, argB.co, argC.co, argD.co])));
	});
var rtfeldman$elm_css$Css$margin4 = rtfeldman$elm_css$Css$prop4('margin');
var rtfeldman$elm_css$Css$marginBottom = rtfeldman$elm_css$Css$prop1('margin-bottom');
var rtfeldman$elm_css$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var rtfeldman$elm_css$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 2, a: a};
};
var rtfeldman$elm_css$Css$pseudoClass = function (_class) {
	return rtfeldman$elm_css$Css$Preprocess$ExtendSelector(
		rtfeldman$elm_css$Css$Structure$PseudoClassSelector(_class));
};
var rtfeldman$elm_css$Css$nthLastChild = function (str) {
	return rtfeldman$elm_css$Css$pseudoClass('nth-last-child(' + (str + ')'));
};
var rtfeldman$elm_css$Css$pointer = {a: 0, co: 'pointer'};
var rtfeldman$elm_css$Css$PxUnits = 0;
var rtfeldman$elm_css$Css$px = A2(rtfeldman$elm_css$Css$Internal$lengthConverter, 0, 'px');
var rtfeldman$elm_css$Css$RemUnits = 0;
var rtfeldman$elm_css$Css$rem = A2(rtfeldman$elm_css$Css$Internal$lengthConverter, 0, 'rem');
var rtfeldman$elm_css$Css$width = rtfeldman$elm_css$Css$prop1('width');
var author$project$Introduction$Groups$itemStyles = function (color) {
	return rtfeldman$elm_css$Html$Styled$Attributes$css(
		_List_fromArray(
			[
				rtfeldman$elm_css$Css$width(
				rtfeldman$elm_css$Css$rem(5)),
				rtfeldman$elm_css$Css$height(
				rtfeldman$elm_css$Css$rem(5)),
				rtfeldman$elm_css$Css$backgroundColor(
				rtfeldman$elm_css$Css$hex(color)),
				rtfeldman$elm_css$Css$borderRadius(
				rtfeldman$elm_css$Css$px(8)),
				rtfeldman$elm_css$Css$color(
				rtfeldman$elm_css$Css$hex('#ffffff')),
				rtfeldman$elm_css$Css$cursor(rtfeldman$elm_css$Css$pointer),
				A4(
				rtfeldman$elm_css$Css$margin4,
				rtfeldman$elm_css$Css$em(0),
				rtfeldman$elm_css$Css$auto,
				rtfeldman$elm_css$Css$em(2),
				rtfeldman$elm_css$Css$auto),
				rtfeldman$elm_css$Css$displayFlex,
				rtfeldman$elm_css$Css$alignItems(rtfeldman$elm_css$Css$center),
				rtfeldman$elm_css$Css$justifyContent(rtfeldman$elm_css$Css$center),
				A2(
				rtfeldman$elm_css$Css$nthLastChild,
				'2',
				_List_fromArray(
					[
						rtfeldman$elm_css$Css$marginBottom(
						rtfeldman$elm_css$Css$px(0))
					]))
			]));
};
var author$project$Introduction$Groups$maybeDraggedItem = F2(
	function (draggable, items) {
		return A2(
			elm$core$Maybe$andThen,
			function (_n0) {
				var dragIndex = _n0.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Groups$system.c_(draggable));
	});
var rtfeldman$elm_css$VirtualDom$Styled$Node = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var rtfeldman$elm_css$VirtualDom$Styled$node = rtfeldman$elm_css$VirtualDom$Styled$Node;
var rtfeldman$elm_css$Html$Styled$node = rtfeldman$elm_css$VirtualDom$Styled$node;
var rtfeldman$elm_css$Html$Styled$div = rtfeldman$elm_css$Html$Styled$node('div');
var rtfeldman$elm_css$VirtualDom$Styled$Unstyled = function (a) {
	return {$: 4, a: a};
};
var rtfeldman$elm_css$VirtualDom$Styled$text = function (str) {
	return rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
		elm$virtual_dom$VirtualDom$text(str));
};
var rtfeldman$elm_css$Html$Styled$text = rtfeldman$elm_css$VirtualDom$Styled$text;
var rtfeldman$elm_css$VirtualDom$Styled$unstyledAttribute = function (prop) {
	return A3(rtfeldman$elm_css$VirtualDom$Styled$Attribute, prop, _List_Nil, '');
};
var rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled = rtfeldman$elm_css$VirtualDom$Styled$unstyledAttribute;
var author$project$Introduction$Groups$draggedItemView = F2(
	function (draggable, items) {
		var _n0 = A2(author$project$Introduction$Groups$maybeDraggedItem, draggable, items);
		if (!_n0.$) {
			var value = _n0.a.co;
			var color = _n0.a.b3;
			return A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					author$project$Introduction$Groups$itemStyles(color),
					A2(
						elm$core$List$map,
						rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
						author$project$Introduction$Groups$system.ej(draggable))),
				_List_fromArray(
					[
						rtfeldman$elm_css$Html$Styled$text(value)
					]));
		} else {
			return rtfeldman$elm_css$Html$Styled$text('');
		}
	});
var author$project$Introduction$Groups$auxiliaryStyles = rtfeldman$elm_css$Html$Styled$Attributes$css(
	_List_fromArray(
		[
			rtfeldman$elm_css$Css$height(rtfeldman$elm_css$Css$auto),
			rtfeldman$elm_css$Css$height(
			rtfeldman$elm_css$Css$rem(3)),
			rtfeldman$elm_css$Css$width(
			rtfeldman$elm_css$Css$rem(10))
		]));
var author$project$Introduction$Groups$gray = 'dimgray';
var rtfeldman$elm_css$VirtualDom$Styled$property = F2(
	function (key, value) {
		return A3(
			rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2(elm$virtual_dom$VirtualDom$property, key, value),
			_List_Nil,
			'');
	});
var rtfeldman$elm_css$Html$Styled$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			elm$json$Json$Encode$string(string));
	});
var rtfeldman$elm_css$Html$Styled$Attributes$id = rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('id');
var author$project$Introduction$Groups$itemView = F4(
	function (model, offset, localIndex, _n0) {
		var group = _n0.t;
		var value = _n0.co;
		var color = _n0.b3;
		var globalIndex = offset + localIndex;
		var itemId = 'id-' + elm$core$String$fromInt(globalIndex);
		var _n1 = _Utils_Tuple2(
			author$project$Introduction$Groups$system.c_(model.z),
			A2(author$project$Introduction$Groups$maybeDraggedItem, model.z, model.A));
		if ((!_n1.a.$) && (!_n1.b.$)) {
			var dragIndex = _n1.a.a.cM;
			var draggedItem = _n1.b.a;
			return ((value === '') && (!_Utils_eq(draggedItem.t, group))) ? A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					A2(
						elm$core$List$cons,
						author$project$Introduction$Groups$auxiliaryStyles,
						A2(
							elm$core$List$map,
							rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
							A2(author$project$Introduction$Groups$system.cN, globalIndex, itemId)))),
				_List_Nil) : (((value === '') && _Utils_eq(draggedItem.t, group)) ? A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					_List_fromArray(
						[author$project$Introduction$Groups$auxiliaryStyles])),
				_List_Nil) : ((!_Utils_eq(dragIndex, globalIndex)) ? A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					A2(
						elm$core$List$cons,
						author$project$Introduction$Groups$itemStyles(color),
						A2(
							elm$core$List$map,
							rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
							A2(author$project$Introduction$Groups$system.cN, globalIndex, itemId)))),
				_List_fromArray(
					[
						rtfeldman$elm_css$Html$Styled$text(value)
					])) : A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					_List_fromArray(
						[
							author$project$Introduction$Groups$itemStyles(author$project$Introduction$Groups$gray)
						])),
				_List_Nil)));
		} else {
			return (value === '') ? A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					_List_fromArray(
						[author$project$Introduction$Groups$auxiliaryStyles])),
				_List_Nil) : A2(
				rtfeldman$elm_css$Html$Styled$div,
				A2(
					elm$core$List$cons,
					rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
					A2(
						elm$core$List$cons,
						author$project$Introduction$Groups$itemStyles(color),
						A2(
							elm$core$List$map,
							rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
							A2(author$project$Introduction$Groups$system.ei, globalIndex, itemId)))),
				_List_fromArray(
					[
						rtfeldman$elm_css$Html$Styled$text(value)
					]));
		}
	});
var author$project$Introduction$Groups$lightBlue = '#88b0ea';
var author$project$Introduction$Groups$lightRed = '#ea9088';
var rtfeldman$elm_css$Css$top = rtfeldman$elm_css$Css$prop1('top');
var author$project$Introduction$Groups$sectionStyles = rtfeldman$elm_css$Html$Styled$Attributes$css(
	_List_fromArray(
		[
			rtfeldman$elm_css$Css$displayFlex,
			rtfeldman$elm_css$Css$alignItems(rtfeldman$elm_css$Css$top),
			rtfeldman$elm_css$Css$justifyContent(rtfeldman$elm_css$Css$center)
		]));
var rtfeldman$elm_css$Html$Styled$section = rtfeldman$elm_css$Html$Styled$node('section');
var elm$virtual_dom$VirtualDom$keyedNodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_keyedNodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles = F2(
	function (_n0, styles) {
		var newStyles = _n0.b;
		var classname = _n0.c;
		return elm$core$List$isEmpty(newStyles) ? styles : A3(elm$core$Dict$insert, classname, newStyles, styles);
	});
var rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute = function (_n0) {
	var val = _n0.a;
	return val;
};
var rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml = F2(
	function (_n6, _n7) {
		var key = _n6.a;
		var html = _n6.b;
		var pairs = _n7.a;
		var styles = _n7.b;
		switch (html.$) {
			case 4:
				var vdom = html.a;
				return _Utils_Tuple2(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					styles);
			case 0:
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n9 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n9.a;
				var finalStyles = _n9.b;
				var vdom = A3(
					elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 1:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n10 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n10.a;
				var finalStyles = _n10.b;
				var vdom = A4(
					elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 2:
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n11 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n11.a;
				var finalStyles = _n11.b;
				var vdom = A3(
					elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n12 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n12.a;
				var finalStyles = _n12.b;
				var vdom = A4(
					elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
		}
	});
var rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml = F2(
	function (html, _n0) {
		var nodes = _n0.a;
		var styles = _n0.b;
		switch (html.$) {
			case 4:
				var vdomNode = html.a;
				return _Utils_Tuple2(
					A2(elm$core$List$cons, vdomNode, nodes),
					styles);
			case 0:
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n2 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n2.a;
				var finalStyles = _n2.b;
				var vdomNode = A3(
					elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 1:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n3 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n3.a;
				var finalStyles = _n3.b;
				var vdomNode = A4(
					elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 2:
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n4 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n4.a;
				var finalStyles = _n4.b;
				var vdomNode = A3(
					elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3(elm$core$List$foldl, rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _n5 = A3(
					elm$core$List$foldl,
					rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _n5.a;
				var finalStyles = _n5.b;
				var vdomNode = A4(
					elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(elm$core$List$cons, vdomNode, nodes),
					finalStyles);
		}
	});
var elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
	});
var rtfeldman$elm_css$VirtualDom$Styled$stylesFromPropertiesHelp = F2(
	function (candidate, properties) {
		stylesFromPropertiesHelp:
		while (true) {
			if (!properties.b) {
				return candidate;
			} else {
				var _n1 = properties.a;
				var styles = _n1.b;
				var classname = _n1.c;
				var rest = properties.b;
				if (elm$core$String$isEmpty(classname)) {
					var $temp$candidate = candidate,
						$temp$properties = rest;
					candidate = $temp$candidate;
					properties = $temp$properties;
					continue stylesFromPropertiesHelp;
				} else {
					var $temp$candidate = elm$core$Maybe$Just(
						_Utils_Tuple2(classname, styles)),
						$temp$properties = rest;
					candidate = $temp$candidate;
					properties = $temp$properties;
					continue stylesFromPropertiesHelp;
				}
			}
		}
	});
var rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties = function (properties) {
	var _n0 = A2(rtfeldman$elm_css$VirtualDom$Styled$stylesFromPropertiesHelp, elm$core$Maybe$Nothing, properties);
	if (_n0.$ === 1) {
		return elm$core$Dict$empty;
	} else {
		var _n1 = _n0.a;
		var classname = _n1.a;
		var styles = _n1.b;
		return A2(elm$core$Dict$singleton, classname, styles);
	}
};
var rtfeldman$elm_css$Css$Structure$ClassSelector = function (a) {
	return {$: 0, a: a};
};
var rtfeldman$elm_css$VirtualDom$Styled$snippetFromPair = function (_n0) {
	var classname = _n0.a;
	var styles = _n0.b;
	return A2(
		rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
		styles,
		rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
			_List_fromArray(
				[
					rtfeldman$elm_css$Css$Structure$ClassSelector(classname)
				])));
};
var rtfeldman$elm_css$VirtualDom$Styled$toDeclaration = function (dict) {
	return rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
		elm$core$List$singleton(
			rtfeldman$elm_css$Css$Preprocess$stylesheet(
				A2(
					elm$core$List$map,
					rtfeldman$elm_css$VirtualDom$Styled$snippetFromPair,
					elm$core$Dict$toList(dict)))));
};
var rtfeldman$elm_css$VirtualDom$Styled$toStyleNode = function (styles) {
	return A3(
		elm$virtual_dom$VirtualDom$node,
		'style',
		_List_Nil,
		elm$core$List$singleton(
			elm$virtual_dom$VirtualDom$text(
				rtfeldman$elm_css$VirtualDom$Styled$toDeclaration(styles))));
};
var rtfeldman$elm_css$VirtualDom$Styled$unstyle = F3(
	function (elemType, properties, children) {
		var unstyledProperties = A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _n0 = A3(
			elm$core$List$foldl,
			rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _n0.a;
		var styles = _n0.b;
		var styleNode = rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(styles);
		return A3(
			elm$virtual_dom$VirtualDom$node,
			elemType,
			unstyledProperties,
			A2(
				elm$core$List$cons,
				styleNode,
				elm$core$List$reverse(childNodes)));
	});
var rtfeldman$elm_css$VirtualDom$Styled$containsKey = F2(
	function (key, pairs) {
		containsKey:
		while (true) {
			if (!pairs.b) {
				return false;
			} else {
				var _n1 = pairs.a;
				var str = _n1.a;
				var rest = pairs.b;
				if (_Utils_eq(key, str)) {
					return true;
				} else {
					var $temp$key = key,
						$temp$pairs = rest;
					key = $temp$key;
					pairs = $temp$pairs;
					continue containsKey;
				}
			}
		}
	});
var rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey = F2(
	function (_default, pairs) {
		getUnusedKey:
		while (true) {
			if (!pairs.b) {
				return _default;
			} else {
				var _n1 = pairs.a;
				var firstKey = _n1.a;
				var rest = pairs.b;
				var newKey = '_' + firstKey;
				if (A2(rtfeldman$elm_css$VirtualDom$Styled$containsKey, newKey, rest)) {
					var $temp$default = newKey,
						$temp$pairs = rest;
					_default = $temp$default;
					pairs = $temp$pairs;
					continue getUnusedKey;
				} else {
					return newKey;
				}
			}
		}
	});
var rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode = F2(
	function (allStyles, keyedChildNodes) {
		var styleNodeKey = A2(rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey, '_', keyedChildNodes);
		var finalNode = rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(allStyles);
		return _Utils_Tuple2(styleNodeKey, finalNode);
	});
var rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed = F3(
	function (elemType, properties, keyedChildren) {
		var unstyledProperties = A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _n0 = A3(
			elm$core$List$foldl,
			rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _n0.a;
		var styles = _n0.b;
		var keyedStyleNode = A2(rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, styles, keyedChildNodes);
		return A3(
			elm$virtual_dom$VirtualDom$keyedNode,
			elemType,
			unstyledProperties,
			A2(
				elm$core$List$cons,
				keyedStyleNode,
				elm$core$List$reverse(keyedChildNodes)));
	});
var rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS = F4(
	function (ns, elemType, properties, keyedChildren) {
		var unstyledProperties = A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _n0 = A3(
			elm$core$List$foldl,
			rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _n0.a;
		var styles = _n0.b;
		var keyedStyleNode = A2(rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, styles, keyedChildNodes);
		return A4(
			elm$virtual_dom$VirtualDom$keyedNodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				elm$core$List$cons,
				keyedStyleNode,
				elm$core$List$reverse(keyedChildNodes)));
	});
var rtfeldman$elm_css$VirtualDom$Styled$unstyleNS = F4(
	function (ns, elemType, properties, children) {
		var unstyledProperties = A2(elm$core$List$map, rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _n0 = A3(
			elm$core$List$foldl,
			rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _n0.a;
		var styles = _n0.b;
		var styleNode = rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(styles);
		return A4(
			elm$virtual_dom$VirtualDom$nodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				elm$core$List$cons,
				styleNode,
				elm$core$List$reverse(childNodes)));
	});
var rtfeldman$elm_css$VirtualDom$Styled$toUnstyled = function (vdom) {
	switch (vdom.$) {
		case 4:
			var plainNode = vdom.a;
			return plainNode;
		case 0:
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A3(rtfeldman$elm_css$VirtualDom$Styled$unstyle, elemType, properties, children);
		case 1:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A4(rtfeldman$elm_css$VirtualDom$Styled$unstyleNS, ns, elemType, properties, children);
		case 2:
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A3(rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed, elemType, properties, children);
		default:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A4(rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS, ns, elemType, properties, children);
	}
};
var rtfeldman$elm_css$Html$Styled$toUnstyled = rtfeldman$elm_css$VirtualDom$Styled$toUnstyled;
var author$project$Introduction$Groups$view = function (model) {
	return rtfeldman$elm_css$Html$Styled$toUnstyled(
		A2(
			rtfeldman$elm_css$Html$Styled$section,
			_List_fromArray(
				[author$project$Introduction$Groups$sectionStyles]),
			_List_fromArray(
				[
					A2(
					rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							author$project$Introduction$Groups$containerStyles(author$project$Introduction$Groups$lightRed)
						]),
					A2(
						elm$core$List$indexedMap,
						A2(
							author$project$Introduction$Groups$itemView,
							model,
							A3(author$project$Introduction$Groups$calculateOffset, 0, 0, model.A)),
						A2(
							elm$core$List$filter,
							function (item) {
								return !item.t;
							},
							model.A))),
					A2(
					rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							author$project$Introduction$Groups$containerStyles(author$project$Introduction$Groups$lightBlue)
						]),
					A2(
						elm$core$List$indexedMap,
						A2(
							author$project$Introduction$Groups$itemView,
							model,
							A3(author$project$Introduction$Groups$calculateOffset, 0, 1, model.A)),
						A2(
							elm$core$List$filter,
							function (item) {
								return item.t === 1;
							},
							model.A))),
					A2(author$project$Introduction$Groups$draggedItemView, model.z, model.A)
				])));
};
var author$project$Introduction$Handle$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '4em')
	]);
var author$project$Introduction$Handle$darkOrange = '#b4752b';
var author$project$Introduction$Handle$handleStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '50px'),
			A2(elm$html$Html$Attributes$style, 'height', '50px'),
			A2(elm$html$Html$Attributes$style, 'background-color', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'margin', '20px'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
		]);
};
var author$project$Introduction$Handle$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '180px'),
			A2(elm$html$Html$Attributes$style, 'height', '100px'),
			A2(elm$html$Html$Attributes$style, 'background-color', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', '#000'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 4em 4em 0')
		]);
};
var author$project$Introduction$Handle$orange = '#dc9a39';
var author$project$Introduction$Handle$draggedItemView = F2(
	function (draggable, fruits) {
		var maybeDraggedFruit = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, fruits));
			},
			author$project$Introduction$Handle$system.c_(draggable));
		if (!maybeDraggedFruit.$) {
			var fruit = maybeDraggedFruit.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Introduction$Handle$itemStyles(author$project$Introduction$Handle$orange),
					author$project$Introduction$Handle$system.ej(draggable)),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						author$project$Introduction$Handle$handleStyles(author$project$Introduction$Handle$darkOrange),
						_List_Nil),
						elm$html$Html$text(fruit)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Handle$darkGreen = '#afb42b';
var author$project$Introduction$Handle$green = '#cddc39';
var author$project$Introduction$Handle$itemView = F3(
	function (draggable, index, fruit) {
		var fruitId = 'id-' + fruit;
		var _n0 = author$project$Introduction$Handle$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(fruitId),
					_Utils_ap(
						author$project$Introduction$Handle$itemStyles(author$project$Introduction$Handle$green),
						A2(author$project$Introduction$Handle$system.cN, index, fruitId))),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						author$project$Introduction$Handle$handleStyles(author$project$Introduction$Handle$darkGreen),
						_List_Nil),
						elm$html$Html$text(fruit)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(fruitId),
					author$project$Introduction$Handle$itemStyles('dimgray')),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(fruitId),
					author$project$Introduction$Handle$itemStyles(author$project$Introduction$Handle$green)),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_Utils_ap(
							author$project$Introduction$Handle$handleStyles(author$project$Introduction$Handle$darkGreen),
							A2(author$project$Introduction$Handle$system.ei, index, fruitId)),
						_List_Nil),
						elm$html$Html$text(fruit)
					]));
		}
	});
var author$project$Introduction$Handle$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Introduction$Handle$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Handle$itemView(model.z),
					model.aT)),
				A2(author$project$Introduction$Handle$draggedItemView, model.z, model.aT)
			]));
};
var author$project$Introduction$Independents$blue = '#118eff';
var author$project$Introduction$Independents$gray = 'dimgray';
var author$project$Introduction$Independents$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '5rem'),
			A2(elm$html$Html$Attributes$style, 'height', '5rem'),
			A2(elm$html$Html$Attributes$style, 'background-color', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 2em 2em 0'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
		]);
};
var author$project$Introduction$Independents$blueView = F3(
	function (draggable, index, item) {
		var itemId = 'blue-' + item;
		var _n0 = author$project$Introduction$Independents$blueSystem.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$blue),
						A2(author$project$Introduction$Independents$blueSystem.cN, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$gray)),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$blue),
						A2(author$project$Introduction$Independents$blueSystem.ei, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Introduction$Independents$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Introduction$Independents$draggedBlue = '#0067c3';
var author$project$Introduction$Independents$draggedBlueView = F2(
	function (draggable, items) {
		var maybeDraggedBlue = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Independents$blueSystem.c_(draggable));
		if (!maybeDraggedBlue.$) {
			var item = maybeDraggedBlue.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$draggedBlue),
					author$project$Introduction$Independents$blueSystem.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Independents$draggedRed = '#c30005';
var author$project$Introduction$Independents$draggedRedView = F2(
	function (draggable, items) {
		var maybeDraggedRed = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Independents$redSystem.c_(draggable));
		if (!maybeDraggedRed.$) {
			var item = maybeDraggedRed.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$draggedRed),
					author$project$Introduction$Independents$redSystem.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Independents$red = '#ff1117';
var author$project$Introduction$Independents$redView = F3(
	function (draggable, index, item) {
		var itemId = 'red-' + item;
		var _n0 = author$project$Introduction$Independents$redSystem.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$red),
						A2(author$project$Introduction$Independents$redSystem.cN, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$gray)),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						author$project$Introduction$Independents$itemStyles(author$project$Introduction$Independents$red),
						A2(author$project$Introduction$Independents$redSystem.ei, index, itemId))),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		}
	});
var author$project$Introduction$Independents$sectionStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '2em')
	]);
var author$project$Introduction$Independents$view = function (model) {
	return A2(
		elm$html$Html$section,
		author$project$Introduction$Independents$sectionStyles,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Introduction$Independents$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Independents$redView(model.ad),
					model.a0)),
				A2(
				elm$html$Html$div,
				author$project$Introduction$Independents$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Independents$blueView(model.R),
					model.aI)),
				A2(author$project$Introduction$Independents$draggedRedView, model.ad, model.a0),
				A2(author$project$Introduction$Independents$draggedBlueView, model.R, model.aI)
			]));
};
var author$project$Introduction$Keyed$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '2em')
	]);
var author$project$Introduction$Keyed$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '5rem'),
			A2(elm$html$Html$Attributes$style, 'height', '5rem'),
			A2(elm$html$Html$Attributes$style, 'background-color', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'margin', '0 2em 2em 0'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
		]);
};
var author$project$Introduction$Keyed$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n2) {
				var dragIndex = _n2.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Keyed$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var _n1 = maybeDraggedItem.a;
			var item = _n1.b;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Introduction$Keyed$itemStyles('#2f804e'),
					author$project$Introduction$Keyed$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Keyed$itemView = F3(
	function (draggable, index, _n0) {
		var key = _n0.a;
		var item = _n0.b;
		var itemId = 'id-' + item;
		var _n1 = author$project$Introduction$Keyed$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? _Utils_Tuple2(
				key,
				A2(
					elm$html$Html$div,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$id(itemId),
						_Utils_ap(
							author$project$Introduction$Keyed$itemStyles('#3da565'),
							A2(author$project$Introduction$Keyed$system.cN, index, itemId))),
					_List_fromArray(
						[
							elm$html$Html$text(item)
						]))) : _Utils_Tuple2(
				key,
				A2(
					elm$html$Html$div,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$id(itemId),
						author$project$Introduction$Keyed$itemStyles('dimgray')),
					_List_Nil));
		} else {
			return _Utils_Tuple2(
				key,
				A2(
					elm$html$Html$div,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$id(itemId),
						_Utils_ap(
							author$project$Introduction$Keyed$itemStyles('#3da565'),
							A2(author$project$Introduction$Keyed$system.ei, index, itemId))),
					_List_fromArray(
						[
							elm$html$Html$text(item)
						])));
		}
	});
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var author$project$Introduction$Keyed$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A3(
				elm$html$Html$Keyed$node,
				'div',
				author$project$Introduction$Keyed$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Keyed$itemView(model.z),
					model.A)),
				A2(author$project$Introduction$Keyed$draggedItemView, model.z, model.A)
			]));
};
var author$project$Introduction$Margins$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Introduction$Margins$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'width', '5rem'),
			A2(elm$html$Html$Attributes$style, 'height', '5rem'),
			A2(elm$html$Html$Attributes$style, 'background-color', color),
			A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
			A2(elm$html$Html$Attributes$style, 'color', 'white'),
			A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2(elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
			A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
		]);
};
var author$project$Introduction$Margins$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n1) {
				var dragIndex = _n1.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Margins$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var item = maybeDraggedItem.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					author$project$Introduction$Margins$itemStyles('#2f804e'),
					author$project$Introduction$Margins$system.ej(draggable)),
				_List_fromArray(
					[
						elm$html$Html$text(item)
					]));
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Margins$itemView = F3(
	function (draggable, index, item) {
		var itemId = 'id-' + item;
		var _n0 = author$project$Introduction$Margins$system.c_(draggable);
		if (!_n0.$) {
			var dragIndex = _n0.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'margin', '2em')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(itemId),
							_Utils_ap(
								author$project$Introduction$Margins$itemStyles('#3da565'),
								A2(author$project$Introduction$Margins$system.cN, index, itemId))),
						_List_fromArray(
							[
								elm$html$Html$text(item)
							]))
					])) : A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'margin', '2em')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(itemId),
							author$project$Introduction$Margins$itemStyles('dimgray')),
						_List_Nil)
					]));
		} else {
			return A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'margin', '2em')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$id(itemId),
							_Utils_ap(
								author$project$Introduction$Margins$itemStyles('#3da565'),
								A2(author$project$Introduction$Margins$system.ei, index, itemId))),
						_List_fromArray(
							[
								elm$html$Html$text(item)
							]))
					]));
		}
	});
var author$project$Introduction$Margins$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Introduction$Margins$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Margins$itemView(model.z),
					model.A)),
				A2(author$project$Introduction$Margins$draggedItemView, model.z, model.A)
			]));
};
var author$project$Introduction$Masonry$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'margin', '0 auto'),
		A2(elm$html$Html$Attributes$style, 'max-width', '40em'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '2em')
	]);
var author$project$Introduction$Masonry$itemStyles = F2(
	function (color, width) {
		return _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'background-color', color),
				A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
				A2(elm$html$Html$Attributes$style, 'flex', '1 0 auto'),
				A2(elm$html$Html$Attributes$style, 'height', '4.5rem'),
				A2(elm$html$Html$Attributes$style, 'margin', '0 1.5em 1.5em 0'),
				A2(
				elm$html$Html$Attributes$style,
				'width',
				elm$core$String$fromInt(width) + 'px')
			]);
	});
var author$project$Introduction$Masonry$draggedItemView = F2(
	function (draggable, items) {
		var maybeDraggedItem = A2(
			elm$core$Maybe$andThen,
			function (_n2) {
				var dragIndex = _n2.cM;
				return elm$core$List$head(
					A2(elm$core$List$drop, dragIndex, items));
			},
			author$project$Introduction$Masonry$system.c_(draggable));
		if (!maybeDraggedItem.$) {
			var _n1 = maybeDraggedItem.a;
			var color = _n1.a;
			var width = _n1.b;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					A2(author$project$Introduction$Masonry$itemStyles, color, width),
					author$project$Introduction$Masonry$system.ej(draggable)),
				_List_Nil);
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Introduction$Masonry$itemView = F3(
	function (draggable, index, _n0) {
		var color = _n0.a;
		var width = _n0.b;
		var itemId = 'id-' + color;
		var _n1 = author$project$Introduction$Masonry$system.c_(draggable);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			return (!_Utils_eq(dragIndex, index)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A2(author$project$Introduction$Masonry$itemStyles, color, width),
						A2(author$project$Introduction$Masonry$system.cN, index, itemId))),
				_List_Nil) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A2(author$project$Introduction$Masonry$itemStyles, 'dimgray', width)),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A2(author$project$Introduction$Masonry$itemStyles, color, width),
						A2(author$project$Introduction$Masonry$system.ei, index, itemId))),
				_List_Nil);
		}
	});
var author$project$Introduction$Masonry$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Introduction$Masonry$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Masonry$itemView(model.z),
					model.A)),
				A2(author$project$Introduction$Masonry$draggedItemView, model.z, model.A)
			]));
};
var author$project$Introduction$Resize$containerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center'),
		A2(elm$html$Html$Attributes$style, 'padding-top', '3em')
	]);
var author$project$Introduction$Resize$handleStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'width', '60px'),
		A2(elm$html$Html$Attributes$style, 'height', '60px'),
		A2(elm$html$Html$Attributes$style, 'color', 'black'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'display', 'flex'),
		A2(elm$html$Html$Attributes$style, 'align-items', 'center'),
		A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
	]);
var author$project$Introduction$Resize$itemStyles = F3(
	function (width, height, color) {
		return _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'border-radius', '8px'),
				A2(elm$html$Html$Attributes$style, 'margin', '0 3em 3em 0'),
				A2(elm$html$Html$Attributes$style, 'display', 'flex'),
				A2(elm$html$Html$Attributes$style, 'align-items', 'flex-start'),
				A2(elm$html$Html$Attributes$style, 'justify-content', 'flex-start'),
				A2(elm$html$Html$Attributes$style, 'background-color', color),
				A2(
				elm$html$Html$Attributes$style,
				'width',
				elm$core$String$fromInt(width) + 'rem'),
				A2(
				elm$html$Html$Attributes$style,
				'height',
				elm$core$String$fromInt(height) + 'rem')
			]);
	});
var author$project$Introduction$Resize$maybeDraggedItem = function (_n0) {
	var draggable = _n0.z;
	var colors = _n0.aM;
	return A2(
		elm$core$Maybe$andThen,
		function (_n1) {
			var dragIndex = _n1.cM;
			return elm$core$List$head(
				A2(elm$core$List$drop, dragIndex, colors));
		},
		author$project$Introduction$Resize$system.c_(draggable));
};
var author$project$Introduction$Resize$draggedItemView = function (model) {
	var _n0 = _Utils_Tuple2(
		author$project$Introduction$Resize$system.c_(model.z),
		author$project$Introduction$Resize$maybeDraggedItem(model));
	if ((!_n0.a.$) && (!_n0.b.$)) {
		var dragIndex = _n0.a.a.cM;
		var targetElement = _n0.a.a.ay;
		var color = _n0.b.a;
		var width = elm$core$Basics$round(targetElement.el.cp);
		var height = elm$core$Basics$round(targetElement.el.b8);
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				A3(author$project$Introduction$Resize$itemStyles, width, height, color),
				_Utils_ap(
					author$project$Introduction$Resize$system.ej(model.z),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Attributes$style,
							'width',
							elm$core$String$fromInt(width) + 'px'),
							A2(
							elm$html$Html$Attributes$style,
							'height',
							elm$core$String$fromInt(height) + 'px'),
							A2(elm$html$Html$Attributes$style, 'transition', 'width 0.5s, height 0.5s')
						]))),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					author$project$Introduction$Resize$handleStyles,
					_List_fromArray(
						[
							elm$html$Html$text('⠶')
						]))
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Introduction$Resize$gray = 'dimgray';
var author$project$Introduction$Resize$itemView = F3(
	function (model, index, _n0) {
		var color = _n0.a;
		var spot = _n0.b;
		var width = spot.cp * 5;
		var itemId = 'id-' + elm$core$String$fromInt(index);
		var height = spot.b8 * 5;
		var _n1 = author$project$Introduction$Resize$system.c_(model.z);
		if (!_n1.$) {
			var dragIndex = _n1.a.cM;
			return (!_Utils_eq(index, dragIndex)) ? A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					_Utils_ap(
						A3(author$project$Introduction$Resize$itemStyles, width, height, color),
						A2(author$project$Introduction$Resize$system.cN, index, itemId))),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						author$project$Introduction$Resize$handleStyles,
						_List_fromArray(
							[
								elm$html$Html$text('⠶')
							]))
					])) : A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A3(author$project$Introduction$Resize$itemStyles, width, height, author$project$Introduction$Resize$gray)),
				_List_Nil);
		} else {
			return A2(
				elm$html$Html$div,
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$id(itemId),
					A3(author$project$Introduction$Resize$itemStyles, width, height, color)),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_Utils_ap(
							author$project$Introduction$Resize$handleStyles,
							A2(author$project$Introduction$Resize$system.ei, index, itemId)),
						_List_fromArray(
							[
								elm$html$Html$text('⠶')
							]))
					]));
		}
	});
var author$project$Introduction$Resize$Spot = F2(
	function (width, height) {
		return {b8: height, cp: width};
	});
var author$project$Introduction$Resize$spots = _List_fromArray(
	[
		A2(author$project$Introduction$Resize$Spot, 1, 1),
		A2(author$project$Introduction$Resize$Spot, 1, 3),
		A2(author$project$Introduction$Resize$Spot, 2, 2),
		A2(author$project$Introduction$Resize$Spot, 2, 1),
		A2(author$project$Introduction$Resize$Spot, 2, 3)
	]);
var author$project$Introduction$Resize$view = function (model) {
	return A2(
		elm$html$Html$section,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				author$project$Introduction$Resize$containerStyles,
				A2(
					elm$core$List$indexedMap,
					author$project$Introduction$Resize$itemView(model),
					A3(
						elm$core$List$map2,
						F2(
							function (color, spot) {
								return _Utils_Tuple2(color, spot);
							}),
						model.aM,
						author$project$Introduction$Resize$spots))),
				author$project$Introduction$Resize$draggedItemView(model)
			]));
};
var author$project$Introduction$Root$demoView = function (model) {
	var _n0 = model.r;
	switch (_n0.$) {
		case 0:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$BasicMsg,
				author$project$Introduction$Basic$view(mo));
		case 1:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$BasicElmUIMsg,
				author$project$Introduction$BasicElmUI$view(mo));
		case 2:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$HandleMsg,
				author$project$Introduction$Handle$view(mo));
		case 3:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$KeyedMsg,
				author$project$Introduction$Keyed$view(mo));
		case 4:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$MarginsMsg,
				author$project$Introduction$Margins$view(mo));
		case 5:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$MasonryMsg,
				author$project$Introduction$Masonry$view(mo));
		case 6:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$ResizeMsg,
				author$project$Introduction$Resize$view(mo));
		case 7:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$IndependentsMsg,
				author$project$Introduction$Independents$view(mo));
		default:
			var mo = _n0.a;
			return A2(
				elm$html$Html$map,
				author$project$Introduction$Root$GroupsMsg,
				author$project$Introduction$Groups$view(mo));
	}
};
var author$project$Introduction$Root$info = function (example) {
	switch (example.$) {
		case 0:
			return {aN: 'Plain sortable list', ae: 'basic', bp: 'Basic'};
		case 1:
			return {aN: 'Designed with mdgriffith/elm-ui', ae: 'basic-elm-ui', bp: 'Basic + Elm UI'};
		case 2:
			return {aN: 'Use a subelement as a drag handle.', ae: 'handle', bp: 'Drag handle'};
		case 3:
			return {aN: 'Use Html.Keyed for optimized DOM updates.', ae: 'keyed', bp: 'Keyed nodes'};
		case 4:
			return {aN: 'Wrap elements in case top or left margins are needed.', ae: 'margins', bp: 'Margins'};
		case 5:
			return {aN: 'Simple horizontal masonry.', ae: 'masonry', bp: 'Masonry'};
		case 6:
			return {aN: 'Put a drag handle to the top-left corner with resizable dragged elements.', ae: 'resize', bp: 'Resize'};
		case 7:
			return {aN: 'Without thinking: duplicate everything.', ae: 'independents', bp: 'Independent lists'};
		default:
			return {aN: 'The list state invariant is that the list has to be gathered by the grouping property, and the auxiliary items have to preserve their places.', ae: 'groups', bp: 'Groupable list + elm-css'};
	}
};
var author$project$Introduction$Root$headerView = function (model) {
	var title = A2(
		elm$core$Basics$composeR,
		author$project$Introduction$Root$info,
		function ($) {
			return $.bp;
		})(model.r);
	var description = A2(
		elm$core$Basics$composeR,
		author$project$Introduction$Root$info,
		function ($) {
			return $.aN;
		})(model.r);
	return A2(
		elm$html$Html$header,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(title)
					])),
				A2(
				elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(description)
					]))
			]));
};
var author$project$Introduction$Root$linkView = function (example) {
	var path = A2(
		elm$url$Url$Builder$absolute,
		_List_fromArray(
			[
				author$project$Base$base,
				'introduction',
				A2(
				elm$core$Basics$composeR,
				author$project$Introduction$Root$info,
				function ($) {
					return $.ae;
				})(example)
			]),
		_List_Nil);
	return A2(
		elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(path)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						A2(
							elm$core$Basics$composeR,
							author$project$Introduction$Root$info,
							function ($) {
								return $.bp;
							})(example))
					]))
			]));
};
var author$project$Introduction$Root$navigationView = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('navigation')
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$h4,
			_List_Nil,
			_List_fromArray(
				[
					elm$html$Html$text('Introduction')
				])),
			A2(
			elm$html$Html$ul,
			_List_Nil,
			A2(
				elm$core$List$map,
				author$project$Introduction$Root$linkView,
				_List_fromArray(
					[
						author$project$Introduction$Root$Basic(author$project$Introduction$Basic$initialModel),
						author$project$Introduction$Root$BasicElmUI(author$project$Introduction$BasicElmUI$initialModel),
						author$project$Introduction$Root$Handle(author$project$Introduction$Handle$initialModel),
						author$project$Introduction$Root$Keyed(author$project$Introduction$Keyed$initialModel),
						author$project$Introduction$Root$Margins(author$project$Introduction$Margins$initialModel),
						author$project$Introduction$Root$Masonry(author$project$Introduction$Masonry$initialModel),
						author$project$Introduction$Root$Resize(author$project$Introduction$Resize$initialModel),
						author$project$Introduction$Root$Independents(author$project$Introduction$Independents$initialModel),
						author$project$Introduction$Root$Groups(author$project$Introduction$Groups$initialModel)
					])))
		]));
var elm$html$Html$h1 = _VirtualDom_node('h1');
var author$project$Main$headerView = A2(
	elm$html$Html$header,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			elm$html$Html$h1,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$a,
					_List_fromArray(
						[
							elm$html$Html$Attributes$href(
							A2(
								elm$url$Url$Builder$absolute,
								_List_fromArray(
									[author$project$Base$base]),
								_List_Nil))
						]),
					_List_fromArray(
						[
							elm$html$Html$text('dnd-list')
						]))
				])),
			A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$a,
					_List_fromArray(
						[
							elm$html$Html$Attributes$href('https://github.com/annaghi/dnd-list')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('GitHub')
						])),
					elm$html$Html$text(' | '),
					A2(
					elm$html$Html$a,
					_List_fromArray(
						[
							elm$html$Html$Attributes$href('https://package.elm-lang.org/packages/annaghi/dnd-list/latest')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Docs')
						]))
				])),
			A2(
			elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					elm$html$Html$text('Drag and Drop for sortable lists in Elm web apps with mouse support')
				]))
		]));
var elm$html$Html$main_ = _VirtualDom_node('main');
var elm$html$Html$nav = _VirtualDom_node('nav');
var author$project$Main$view = function (model) {
	return {
		dY: _List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('sidebar')
					]),
				_List_fromArray(
					[
						author$project$Main$headerView,
						A2(
						elm$html$Html$nav,
						_List_Nil,
						_List_fromArray(
							[
								A2(elm$html$Html$map, author$project$Main$IntroductionMsg, author$project$Introduction$Root$navigationView),
								A2(elm$html$Html$map, author$project$Main$ConfigurationMsg, author$project$Configuration$Root$navigationView),
								A2(elm$html$Html$map, author$project$Main$GalleryMsg, author$project$Gallery$Root$navigationView)
							]))
					])),
				A2(
				elm$html$Html$main_,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('main')
					]),
				function () {
					var _n0 = model.r;
					switch (_n0.$) {
						case 0:
							return _List_fromArray(
								[
									elm$html$Html$text('Not found')
								]);
						case 1:
							var mo = _n0.a;
							return _List_fromArray(
								[
									A2(
									elm$html$Html$map,
									author$project$Main$HomeMsg,
									author$project$Home$view(mo))
								]);
						case 2:
							var mo = _n0.a;
							return _List_fromArray(
								[
									A2(
									elm$html$Html$map,
									author$project$Main$IntroductionMsg,
									author$project$Introduction$Root$headerView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$IntroductionMsg,
									author$project$Introduction$Root$demoView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$IntroductionMsg,
									author$project$Introduction$Root$codeView(mo))
								]);
						case 3:
							var mo = _n0.a;
							return _List_fromArray(
								[
									A2(
									elm$html$Html$map,
									author$project$Main$ConfigurationMsg,
									author$project$Configuration$Root$headerView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$ConfigurationMsg,
									author$project$Configuration$Root$demoView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$ConfigurationMsg,
									author$project$Configuration$Root$codeView(mo))
								]);
						default:
							var mo = _n0.a;
							return _List_fromArray(
								[
									A2(
									elm$html$Html$map,
									author$project$Main$GalleryMsg,
									author$project$Gallery$Root$headerView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$GalleryMsg,
									author$project$Gallery$Root$demoView(mo)),
									A2(
									elm$html$Html$map,
									author$project$Main$GalleryMsg,
									author$project$Gallery$Root$codeView(mo))
								]);
					}
				}())
			]),
		bp: 'annaghi | dnd-list'
	};
};
var elm$browser$Browser$application = _Browser_application;
var author$project$Main$main = elm$browser$Browser$application(
	{eA: author$project$Main$init, eN: author$project$Main$UrlChanged, eO: author$project$Main$LinkClicked, ds: author$project$Main$subscriptions, dz: author$project$Main$update, fv: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(0))(0)}});}(this));