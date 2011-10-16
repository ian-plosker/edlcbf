-module(dlht).
-author("Ian Plosker").

-export([dlht/3,
         add/2, member/2]).

-record(dlht, {
    count = 0, % number of elements
    d,     % number of tables
    b,     % number of buckets per table
    l,     % fingerprints per bucket
    tables
}).

-record(bucket, {
    count,
    zero_count,
    fingerprints
}).

-import(erlang, [make_tuple/2, phash2/2]).

dlht(D, B, L) ->
    #dlht{
        count=0,
        d=D,
        b=B,
        l=L,
        tables = make_tuple(
                    D,
                    make_tuple(
                        B,
                        #bucket{
                                count=0,
                                zero_count=0,
                                fingerprints=[]
                               }
                    )
                 )
    }.

add(Element, DLHT) ->
    Hash = make_hash(Element),
    Targets = get_target_bucket_indexes(Hash, DLHT#dlht.b),
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
                                    fingerprints=[Hash|Bucket#bucket.fingerprints]
                                   }
                  )
              )
             }.

member(Element, DLHT) ->
    Hash = make_hash(Element),
    Targets = get_target_bucket_indexes(Hash, DLHT#dlht.b),
    lists:any(fun({TableIndex, BucketIndex}) -> 
        Table = element(TableIndex, DLHT#dlht.tables),
        Bucket = element(BucketIndex, Table),
        lists:member(Hash, Bucket#bucket.fingerprints)
    end, Targets).

make_hash(Element) ->
    phash2(Element, 4294967296).

get_target_bucket_indexes(Hash, B) ->
    get_target_bucket_indexes(<<Hash:32>>, B, B).

get_target_bucket_indexes(_, _, 0) -> [];
get_target_bucket_indexes(Hash, B, N) ->
    Length = trunc(math:log(B) / math:log(2)),
    <<TargetBucket:Length,Rest/bitstring>> = Hash,
    [{B - N + 1, TargetBucket + 1}|get_target_bucket_indexes(Rest, B, N - 1)].
