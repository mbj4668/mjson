# mjson - An Erlang library for encoding and decoding JSON

Small (less than 100 lines each) and quite performant JSON decoder and
encoder.  No external dependencies.  Works on AtomVM and OTP >= R16.

RFC 8259 compliant.

Passes all tests in https://github.com/nst/JSONTestSuite.

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
