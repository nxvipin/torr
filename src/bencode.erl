-module(bencode).
-export([decode/1]).

-type bint() :: integer().
-type bstr() :: binary().
-type blist() :: list(bdecoded()).
-type bdict() :: #{binary() => bdecoded()}.
-type bdecoded() :: bint() | bstr() | blist() | bdict().

-spec decode(binary()) -> bdecoded().
decode(Data) ->
    {Result, _} = decode_(Data),
    Result.

-spec decode_(binary()) -> {bdecoded(), binary()}.
decode_(<<$l, Data/binary>>) ->
    decode_list(Data, []);

decode_(<<$d, Data/binary>>) ->
    decode_dict(Data, maps:new());

decode_(<<$i, Data/binary>>) ->
    decode_integer(Data);

decode_(Data) ->
    decode_string(Data).

-spec decode_dict(binary(), map()) -> {bdict(), binary()}.
decode_dict(<<$e, Data/binary>>, Acc) ->
    {Acc, Data};

decode_dict(Data, Acc) ->
    {Key, Tail_} = decode_string(Data),
    {Value, Tail__} = decode_(Tail_),
    decode_dict(Tail__, maps:put(Key, Value, Acc)).

-spec decode_list(binary(), [bdecoded()]) -> {[bdecoded()], binary()}.
decode_list(<<$e, Data/binary>>, Acc) ->
    {lists:reverse(Acc), Data};

decode_list(Data, Acc) ->
    {Value, Tail} = decode_(Data),
    decode_list(Tail, [Value | Acc]).

-spec decode_integer(binary()) -> {bint(), binary()}.
decode_integer(Data) ->
    [Value, Tail] = binary:split(Data, <<$e>>),
    case Value of
        <<$0, _>> -> erlang:error(badarg);
        <<$-, $0>> -> erlang:error(badarg);
        <<$-, $0, _>> -> erlang:error(badarg);
        _ -> ok
    end,
    {binary_to_integer(Value), Tail}.

-spec decode_string(binary()) -> {bstr(), binary()}.
decode_string(Data) ->
    [StrLength_, Tail_] = binary:split(Data, <<":">>),
    StrLength = binary_to_integer(StrLength_),
    case Tail_ of
        <<Str:StrLength/binary, Tail/binary>> ->
            {Str, Tail};
        _ -> {Tail_, <<>>}
    end.
