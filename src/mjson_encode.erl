%%% Small (< 100 lines), simple, and quite fast JSON encoder, RFC8259 compatible
-module(mjson_encode).
-export([encode/2]).

-record(cfg, {
          nl = $\n
        , sp = $\s
        , sort_objects
        , float_opts
        }).

encode(Val, Opts) ->
    {Indent, Cfg} = popts(Opts),
    enc_val(Val, Indent, Cfg).

popts(Opts) ->
    FloatOpts = case list_to_integer(erlang:system_info(otp_release)) of
                    Major when Major >= 25 -> [short];
                    _ -> []
                end,
    Cfg0 = #cfg{sort_objects = maps:get(sort_objects, Opts, false),
                float_opts = FloatOpts},
    case maps:get(compact, Opts, false) of
        true ->
            {<<>>, Cfg0#cfg{nl = [], sp = []}};
        false ->
            {[maps:get(indent, Opts, [])], Cfg0}
        end.

enc_val(Val, Ind, C) when is_map(Val) ->
    case maps:size(Val) of
        0 ->
            [<<"{}">>];
        _ ->
            NInd = more_indent(Ind),
            [${, C#cfg.nl, enc_members(Val, NInd, C), C#cfg.nl, Ind, $}]
    end;
enc_val([], _Ind, _C) ->
    [<<"[]">>];
enc_val(Val, Ind, C) when is_list(Val) ->
    NInd = more_indent(Ind),
    [$[, C#cfg.nl, enc_elements(Val, NInd, C), C#cfg.nl, Ind, $]];
enc_val(Val, _Ind, _C) when is_atom(Val) ->
    case Val of
        true  -> <<"true">>;
        false -> <<"false">>;
        null  -> <<"null">>;
        _     -> enc_str(atom_to_binary(Val))
    end;
enc_val(Val, _Ind, _C) when is_binary(Val) ->
    enc_str(Val);
enc_val(Val, _Ind, _C) when is_integer(Val) ->
    [integer_to_binary(Val)];
enc_val(Val, _Ind, C) when is_float(Val) ->
    [float_to_binary(Val, C#cfg.float_opts)].

enc_members(Val, NInd, C) ->
    Pairs0 = [{member_name_to_bin(Name), V} || {Name, V} <- maps:to_list(Val)],
    Pairs1 =
        if C#cfg.sort_objects -> lists:keysort(1, Pairs0);
           true               -> Pairs0
        end,
    lists:join([$,, C#cfg.nl],
               [[NInd, enc_str(Name), $:, C#cfg.sp, enc_val(V, NInd, C)]
                || {Name, V} <- Pairs1]).

member_name_to_bin(Val) when is_atom(Val) -> atom_to_binary(Val);
member_name_to_bin(Val)                   -> iolist_to_binary(Val).

enc_elements(Val, NInd, C) ->
    lists:join([$,, C#cfg.nl], [[NInd, enc_val(V, NInd, C)] || V <- Val]).

-ifndef(NO_RE_RUN_2). % not in AtomVM
enc_str(Bin) ->
    case re:run(Bin, <<"\"|\\\\|[\\x{0000}-\\x{001F}]">>) of
        nomatch -> [$", Bin, $"];
        _       -> [$", escape_str(Bin), $"]
    end.
-else.
enc_str(Bin) ->
    [$", escape_str(Bin), $"].
-endif.

escape_str(<<$", T/binary>>)  -> [$\\, $" | escape_str(T)];
escape_str(<<$\\, T/binary>>) -> [$\\, $\\ | escape_str(T)];
escape_str(<<C, T/binary>>) when C =< 16#1F ->
    case C of
        $\t           -> [$\\, $t | escape_str(T)];
        $\n           -> [$\\, $n | escape_str(T)];
        $\r           -> [$\\, $r | escape_str(T)];
        _ when C < 16 -> ["\\u000", integer_to_binary(C, 16) | escape_str(T)];
        _             -> ["\\u00", integer_to_binary(C, 16) | escape_str(T)]
    end;
escape_str(<<C, T/binary>>) ->
    [C | escape_str(T)];
escape_str(<<>>) ->
    [].

more_indent(<<>>) -> <<>>;
more_indent(Ind) -> [<<"  ">> | Ind].
