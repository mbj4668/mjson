-module(mjson).
-export([decode/1, encode/1, encode/2]).

-export_type([val/0, enc_val/0]).
-export_type([obj/0, array/0, num/0, str/0, literal/0]).
-export_type([enc_obj/0, enc_str/0]).
-export_type([encode_opts/0]).

%% Represents a JSON value.
-type val() :: obj() | array() | num() | str() | literal().

%% Represents a JSON value for encoding. Just like a `val()`, but also
%% accepts atoms as strings, and iodata as object member names.
-type enc_val() :: val() | enc_obj() | enc_str().

%% Represents a JSON object.
-type obj()     :: #{str() => val()}.

%% Represents a JSON array.
-type array()   :: [val()].

%% Represents a JSON string.
-type str()     :: binary().

%% Represents a JSON number.
-type num()     :: number().

%% Represents a JSON literal.
-type literal() :: true | false | null.

%% When encoding, object member names can be atoms and iodata.
-type enc_obj() :: #{enc_str() | iodata() => enc_val()}.

%% When encoding, atoms and binaries will be encoded as JSON strings.
-type enc_str() :: binary() | atom().

%% Options for `encode/2`.
-type encode_opts() ::
        #{
           %% Set to `true` to encode without any whitespace.
           compact => boolean()

           %% Indent all rows with this whitespace.
         , indent => iodata()

           %% Print object members sorted by their name.
         , sort_objects => boolean()
         }.

-spec encode(enc_val()) -> iodata().
encode(Val) ->
    encode(Val, #{}).

-spec encode(enc_val(), encode_opts()) -> iodata().
encode(Val, Opts) ->
    mjson_encode:encode(Val, Opts).

-spec decode(binary()) -> {ok, val()}.
%% If `Bin` does not contain valid JSON, `decode/1` crashes.
decode(Bin) when is_binary(Bin) ->
    mjson_decode:decode(Bin).

