-module(mjson_tests).

-include_lib("eunit/include/eunit.hrl").

test_file_path(File) ->
    F1 = filename:join(["test", File]),
    case filelib:is_file(F1) of
        true -> F1;
        _ -> filename:join(["..", "test", File])
    end.

plain_parse_test() ->
    {ok, Bin} = file:read_file(test_file_path("test1.json")),
    ?assertEqual(
       {ok, #{<<"lists">> => [42, 1.001],
              <<"maps">> => #{
                              <<"nosuchatom">> => null,
                              <<"string">> => <<"">>
                             },
              <<"xxbool1">> => false,
              <<"true">> => true
             }},
       mjson:decode(Bin)).

parse_existing_atom_test() ->
    {ok, Bin} = file:read_file(test_file_path("test1.json")),
    ?assertEqual(
       {ok, #{lists => [42, 1.001],
              maps => #{
                        <<"nosuchatom">> => null,
                        string => <<"">>
                       },
              <<"xxbool1">> => false,
              true => true
             }},
       mjson:decode(Bin, #{key_as_existing_atom => true})).

plain_encode_test() ->
    {ok, Bin} = file:read_file(test_file_path("test1.json")),
    {ok, Term1} = mjson:decode(Bin),
    ?assertEqual(
       {ok, Term1},
       mjson:decode(iolist_to_binary(mjson:encode(Term1)))).

compace_encode_test() ->
    {ok, Bin} = file:read_file(test_file_path("test1.json")),
    {ok, Term1} = mjson:decode(Bin),
    ?assertEqual(
       {ok, Term1},
       mjson:decode(iolist_to_binary(mjson:encode(Term1, #{compact => true})))).

encode_existing_atom_test() ->
    {ok, Bin} = file:read_file(test_file_path("test1.json")),
    {ok, Term2} = mjson:decode(Bin, #{key_as_existing_atom => true}),
    ?assertEqual(
       {ok, Term2},
       mjson:decode(iolist_to_binary(mjson:encode(Term2)),
                    #{key_as_existing_atom => true})).

