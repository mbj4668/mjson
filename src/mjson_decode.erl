%%% Small (< 100 lines), simple, and quite fast JSON decoder, RFC8259 compatible
-module(mjson_decode).
-export([decode/2]).

-define(isint(C), ((C) >= $0 andalso (C) =< $9)).
-define(isexp(C), ((C) == $e orelse (C) == $E)).
-define(issign(C), ((C) == $+ orelse (C) == $-)).
-define(isws(C), ((C) == $\s orelse (C) == $\t
                  orelse (C) == $\r orelse (C) == $\n)).

decode(Chars, Opts) when is_binary(Chars) ->
    {Value, T} = dec_value(skip_ws(Chars), Opts),
    <<>> = skip_ws(T),
    {ok, Value}.

dec_value(<<$", T/binary>>, _)               -> dec_str(T, []);
dec_value(<<${, T/binary>>, Opts)            -> dec_object(skip_ws(T), Opts);
dec_value(<<$[, T/binary>>, Opts)            -> dec_array(skip_ws(T), Opts);
dec_value(<<"true", T/binary>>, _)           -> {true, T};
dec_value(<<"false", T/binary>>, _)          -> {false, T};
dec_value(<<"null", T/binary>>, _)           -> {null, T};
dec_value(<<$-, T/binary>>, _)               -> dec_number(T, [$-]);
dec_value(<<C, T/binary>>, _) when ?isint(C) -> dec_number(T, [C]).

dec_str(<<$", T/binary>>, Acc)     -> {iolist_to_binary(Acc), T};
dec_str(<<$\\, C, T/binary>>, Acc) ->
    if C == $b -> dec_str(T, [Acc, $\b]);
       C == $f -> dec_str(T, [Acc, $\f]);
       C == $n -> dec_str(T, [Acc, $\n]);
       C == $r -> dec_str(T, [Acc, $\r]);
       C == $t -> dec_str(T, [Acc, $\t]);
       C == $u -> dec_escaped_unicode(T, Acc);
       C == $" orelse C == $\\ orelse C == $/ -> dec_str(T, [Acc, C])
    end;
dec_str(Bin, Acc) ->
    Len = str_count(Bin, 0),
    <<Part:Len/binary, T/binary>> = Bin,
    dec_str(T, [Acc, Part]).

str_count(<<C, _/binary>>, Pos) when Pos > 0, C == $" orelse C == $\\ -> Pos;
str_count(<<C, T/binary>>, Pos) when C > 16#1F -> str_count(T, Pos+1).

-ifndef(NO_LIST_TO_INTEGER_2). % not in AtomVM
dec_escaped_unicode(<<X0,X1,X2,X3, T1/binary>>, Acc) ->
    case list_to_integer([X0,X1,X2,X3], 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            <<$\\,$u,L0,L1,L2,L3, T2/binary>> = T1,
            Low = list_to_integer([L0,L1,L2,L3], 16), % no validation of low
            <<C/utf16>> = <<High:16, Low:16>>,
            dec_str(T2, [Acc, <<C/utf8>>]);
        C ->
            dec_str(T1, [Acc, <<C/utf8>>])
    end.
-else.
dec_escaped_unicode(T, Acc) -> % in this case don't unescape
    dec_str(T, [Acc, $u]).
-endif.

dec_number(<<C, _/binary>>, Acc)
  when ?isint(C) andalso (Acc == [$0] orelse Acc == [$0, $-]) -> error(badarg);
dec_number(<<C, T/binary>>, Acc) when ?isint(C) -> dec_number(T, [C | Acc]);
dec_number(<<$., T/binary>>, Acc)               -> dec_float(T, [$. | Acc]);
dec_number(<<C, T/binary>>, Acc) when ?isexp(C) -> dec_float(T, [C,$0,$.| Acc]);
dec_number(T, Acc) -> {list_to_integer(lists:reverse(Acc)), T}.

dec_float(<<C, T/binary>>, Acc)
  when ?isint(C) orelse ?isexp(C) orelse ?issign(C) -> dec_float(T, [C | Acc]);
dec_float(T, Acc) -> {list_to_float(lists:reverse(Acc)), T}.

dec_object(<<$}, T/binary>>, _) -> {#{}, T};
dec_object(T, Opts)             -> dec_members(T, #{}, Opts).

dec_members(<<$", T/binary>>, Acc0, Opts) ->
    {Str, T2} = dec_str(T, []),
    Name = maybe_existing_atom(Str, Opts),
    <<$:, T3/binary>> = skip_ws(T2),
    {Val, T4} = dec_value(skip_ws(T3), Opts),
    Acc1 = Acc0#{Name => Val},
    case skip_ws(T4) of
        <<$,, T5/binary>> -> dec_members(skip_ws(T5), Acc1, Opts);
        <<$}, T5/binary>> -> {Acc1, T5}
    end.

maybe_existing_atom(Str, #{key_as_existing_atom := true}) ->
    try binary_to_existing_atom(Str) catch _:_ -> Str end;
maybe_existing_atom(Str, _) -> Str.

dec_array(<<$], T/binary>>, _) -> {[], T};
dec_array(T, Opts)             -> dec_elements(T, Opts, []).

dec_elements(T, Opts, Acc0) ->
    {Value, T2} = dec_value(skip_ws(T), Opts),
    Acc1 = [Value | Acc0],
    case skip_ws(T2) of
        <<$,, T3/binary>> -> dec_elements(skip_ws(T3), Opts, Acc1);
        <<$], T3/binary>> -> {lists:reverse(Acc1), T3}
    end.

skip_ws(<<C, T/binary>>) when ?isws(C) -> skip_ws(T);
skip_ws(T) -> T.
