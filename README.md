# mjson - An Erlang library for encoding and decoding JSON

Small (less than 100 lines each) and quite performant JSON decoder and
encoder.  No external dependencies.  Works on AtomVM and OTP >= R16.

RFC 8259 compliant.

Passes all tests in https://github.com/nst/JSONTestSuite.

Useful when a simple, basic JSON encoder / decoder is needed.

# Erlang representation of JSON

JSON type    | Erlang type
-------------|----------------------------------
`object`     | `map`
`array`      | `list`
`string`     | `binary`
`number`     | `integer \| float`
`literal`    | `atom`

Note that there are three JSON literals `true`, `false`, and `null`,
and they are decoded into the corresponding atoms `'true'`, `'false'`,
and `'null'`.

For example, the following JSON value:
```json
{
  "id": 198030,
  "sizes": [7.0, 12.0],
  "valid": false
}
```
is decoded to the Erlang term:
```erlang
#{
  <<"id">> => 19830,
  <<"sizes">> => [7.0, 12.0],
  <<"valid">> => false
 }
```

If the option `key_as_existing_atom` is given when decoding, object
member names are converted to existing atoms.  If the atom doesn't
exist, the object member name is returned as a binary.

The `key_as_existing_atom` option is useful when the schema for the
JSON document is known, and all keys are already present in the code.

## Encoding

In addition to the representation described above, the encoder
encodes Erlang atoms into JSON strings (except for the atoms `'true'`,
`'false'`, and `'null'`), and also encodes Erlang map keys of type
iodata to JSON strings.

For example, the following Erlang term:
```erlang
#{
  "bar" => baz,
  foo => null
 }
```
is encoded to:
```json
{
  "bar": "baz",
  "foo": null
}
```
which is decoded to:
```erlang
#{
  <<"bar">> => <<"baz">>,
  <<"foo">> => null
 }
```

# API documentation

See [mjson.md](doc/mjson.md).

# Build for AtomVM

AtomVM doesn't implement `erlang:list_to_integer/2` and the module
`re`.  Define the variables below to compile without using these
functions.  Without `erlang:list_to_integer/2`, `mjson:decode/1` will
not translate unicode-escaped characters.

```
ERLC_OPTS="-D NO_LIST_TO_INTEGER_2 -D NO_RE_RUN_2" make
```
