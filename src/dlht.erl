-module(dlht).
-author("Ian Plosker").

%-export([dlht/2,
%add/2, member/2]).

%-export([bitstring_any/3, bitstring_member/2]).

-compile(export_all).

-record(dlht, {
    count = 0, % number of elements
    d,     % number of tables
    b,     % number of buckets per table
    %l,     % fingerprints per bucket
    tables
}).

-record(bucket, {
    count,
    zero_count,
    fingerprints
}).

-import(erlang, [make_tuple/2, phash2/2]).

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl")i
    -include_lib("eqc/include/eqc.hrl").
-endif.


dlht(D, B) -> %, L) ->
    #dlht{
        count=0,
        d=D,
        b=B,
        %l=L,
        tables = make_tuple(
                    D,
                    make_tuple(
                        B,
                        #bucket{
                                count=0,
                                zero_count=0,
                                fingerprints= <<>>
                               }
                    )
                 )
    }.

add(Element, DLHT) ->
    Hash = <<(make_hash(Element)):32>>,
    Targets = get_target_bucket_indexes(Hash, DLHT#dlht.d, DLHT#dlht.b),
    {TableIndex, _} = lists:foldl(fun({TableIndex, BucketIndex}, {LowTable, LowCount}) ->
        Table = element(TableIndex, DLHT#dlht.tables),
        Bucket = element(BucketIndex, Table),
        Count = Bucket#bucket.count,
        case TableIndex == 1 orelse Count < LowCount of
            true -> {TableIndex, Count};
            false -> {LowTable, LowCount}
        end
    end, {1, 0}, Targets),
    Table = element(TableIndex, DLHT#dlht.tables),
    {TableIndex, BucketIndex} = lists:nth(TableIndex, Targets),
    Bucket = element(BucketIndex, Table),
    Length = trunc(math:log(DLHT#dlht.b) / math:log(2)),
    DLHT#dlht{
              count=DLHT#dlht.count + 1,
              tables=setelement(
                  TableIndex,
                  DLHT#dlht.tables,
                  setelement(
                      BucketIndex,
                      Table,
                      Bucket#bucket{
                                    count=Bucket#bucket.count + 1,
                                    fingerprints= <<(Bucket#bucket.fingerprints)/binary,(bitstring_remove(Length*(TableIndex-1), Length, <<Hash/bitstring>>)):(32-Length)/bitstring>>
                                   }
                  )
              )
             }.

member(Element, DLHT) ->
    Hash = <<(make_hash(Element)):32>>,
    Targets = get_target_bucket_indexes(Hash, DLHT#dlht.d, DLHT#dlht.b),
    Length = trunc(math:log(DLHT#dlht.b) / math:log(2)),
    lists:any(fun({TableIndex, BucketIndex}) ->
        Table = element(TableIndex, DLHT#dlht.tables),
        Bucket = element(BucketIndex, Table),
        bitstring_member(<<(bitstring_remove(Length*(TableIndex-1), Length, Hash)):(bit_size(Hash) - Length)/bitstring>>, Bucket#bucket.fingerprints)
    end, Targets).

make_hash(Element) ->
    phash2({Element}, 4294967296).

get_target_bucket_indexes(<<Hash/bitstring>>, D, B) ->
    lists:reverse(get_target_bucket_indexes(Hash, D, B, [])).

get_target_bucket_indexes(<<_/bitstring>>, 0, _B, Acc) -> Acc;
get_target_bucket_indexes(<<Hash/bitstring>>, D, B, Acc) ->
    Length = trunc(math:log(B) / math:log(2)),
    <<TargetBucket:Length,Rest/bitstring>> = Hash,
    get_target_bucket_indexes(Rest, D - 1, B, [{length(Acc) + 1, TargetBucket + 1}|Acc]).

bitstring_any(MatchFun, Window, Bin) when bit_size(Bin) >= Window ->
    <<Element:Window/bitstring, Rest/bitstring>> = Bin,
    case MatchFun(Element) of
        true -> true;
        false -> bitstring_any(MatchFun, Window, Rest)
    end;
bitstring_any(_, _, _) -> false.

bitstring_member(<<_/bitstring>> = Element, <<_/bitstring>> = Bin) ->
    bitstring_any(fun(X) -> X == Element end, bit_size(Element), Bin).

bitstring_remove(Start, Num, Bin) ->
    <<Beginning:Start/bitstring, _:Num/bitstring, Rest/bitstring>> = Bin,
    bitstring_join(Beginning, Rest).

bitstring_join(One, Two) ->
    <<One/bitstring, Two/bitstring>>.

-ifdef(TEST).

    basic_test() ->
        D = dlht(3,16),
        Da = add(a, D),
        Db = add(b, Da),
        Dc = add(c, Db),
        ?assert(member(a, Dc)),
        ?assert(member(b, Dc)),
        ?assert(member(c, Dc)),
        ?assertNot(member(d, Dc)),
        ?assertNot(member(e, Dc)),
        ?assertNot(member(f, Dc)).

    pos_int() ->
        ?LET(N, int(), abs(N) + 1).

    power_of_two() ->
        ?LET(N, pos_int(), begin trunc(math:pow(2, N)) end).

    prop_add_are_members() ->
        ?FORALL(L, non_empty(list(int())),
            ?FORALL({D, B}, {pos_int(), power_of_two()},
                ?IMPLIES(D * B =< 64,
                    check_membership(L, D, B)))).

    prop_remove_bin() ->
        ?FORALL(L, non_empty(list(int())),
            ?FORALL({D, B}, {pos_int(), power_of_two()},
                ?IMPLIES(D * B =< 64,
                    check_membership(L, D, B)))).

    check_membership(L, D, B) ->
        F = lists:foldl(fun(X, Acc) -> add(X, Acc) end, dlht(D, B), L),
        lists:all(fun(X) -> member(X, F) end, L).

-endif.
