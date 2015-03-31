-module(bencode_tests).
-include_lib("eunit/include/eunit.hrl").

%% TODO: Smarter tests!

decode_string_test() ->
    ?assertEqual(<<>>, bencode:decode(<<"0:">>)),
    ?assertEqual(<<"abc">>, bencode:decode(<<"3:abc">>)).

decode_int_test() ->
    ?assertEqual(0, bencode:decode(<<"i0e">>)),
    ?assertEqual(-1, bencode:decode(<<"i-1e">>)),
    ?assertError(badarg, bencode:decode(<<"i00e">>)),
    ?assertError(badarg, bencode:decode(<<"i-0e">>)),
    ?assertError(badarg, bencode:decode(<<"i-01e">>)).

decode_list_test() ->
    ?assertEqual([], bencode:decode(<<"le">>)),
    ?assertEqual([<<>>], bencode:decode(<<"l0:e">>)),
    ?assertEqual([0], bencode:decode(<<"li0ee">>)),
    ?assertEqual([[]], bencode:decode(<<"llee">>)),
    ?assertEqual([[[]],[<<>>]], bencode:decode(<<"llleel0:ee">>)),
    ?assertEqual([[]], bencode:decode(<<"llee">>)),
    ?assertEqual([[]], bencode:decode(<<"llee">>)).

decode_dict_test() ->
    ?assertEqual(#{}, bencode:decode(<<"de">>)),
    ?assertEqual(#{<<"a">> => <<"b">>}, bencode:decode(<<"d1:a1:be">>)),
    ?assertEqual(#{<<"a">> => [#{<<"x">> => <<>>}, <<"yz">>, 256]},
                 bencode:decode(<<"d1:ald1:x0:e2:yzi256eee">>)).

encode_string_test() ->
    ?assertEqual(<<"0:">>, bencode:encode(<<>>)),
    ?assertEqual(<<"3:abc">>, bencode:encode(<<"abc">>)).

encode_int_test() ->
    ?assertEqual(<<"i0e">>, bencode:encode(0)),
    ?assertEqual(<<"i-1e">>, bencode:encode(-1)).

encode_list_test() ->
    ?assertEqual(<<"le">>, bencode:encode([])),
    ?assertEqual(<<"l0:e">>, bencode:encode([<<>>])),
    ?assertEqual(<<"li0ee">>, bencode:encode([0])),
    ?assertEqual(<<"llee">>, bencode:encode([[]])),
    ?assertEqual(<<"llleel0:ee">>, bencode:encode([[[]],[<<>>]])),
    ?assertEqual(<<"llee">>, bencode:encode([[]])).

encode_dict_test() ->
    ?assertEqual(<<"de">>, bencode:encode(#{})),
    ?assertEqual(<<"d1:a1:be">>, bencode:encode(#{<<"a">> => <<"b">>})),
    ?assertEqual(<<"d1:ald1:x0:e2:yzi256eee">>,
                 bencode:encode(#{<<"a">> => [#{<<"x">> => <<>>}, <<"yz">>, 256]})).
