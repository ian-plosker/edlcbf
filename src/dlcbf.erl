%% -------------------------------------------------------------------
%%
%% dlcbf: Erlang NIF wrapper for d-left counting bloom filter
%%
%% Copyright (c) 2012 Basho Technologies, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(dlcbf).
-author("Ian Plosker").
-author("Steve Vinoski").

-export([new/2,
         add/2,
         delete/2,
         in/2]).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-export([pos_int/0,
         power_of_two/0,
         prop_add_are_members/0,
         check_membership/4,
         keys/0,
         ops/1,
         apply_ops/3,
         prop_add_delete/0]).
-endif.
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-spec init() -> ok | {error, any()}.
init() ->
    case code:priv_dir(dlht) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "dlcbf"]);
                _ ->
                    SoName = filename:join("../priv", "dlcbf")
            end;
         Dir ->
            SoName = filename:join(Dir, "dlcbf")
    end,
    erlang:load_nif(SoName, 0).

-spec new(pos_integer(), pos_integer()) -> {ok, reference()}.
new(_D, _B) ->
    ?nif_stub.

-spec add(binary(), reference()) -> ok.
add(_Bin, _Dlht) ->
    ?nif_stub.

-spec delete(binary(), reference()) -> ok.
delete(_Bin, _Dlht) ->
    ?nif_stub.

-spec in(binary(), reference()) -> boolean().
in(_Bin, _Dlht) ->
    ?nif_stub.


-ifdef(TEST).

basic_test() ->
    {ok, D} = new(2,16),
    ok = add(<<"a">>, D),
    ok = add(<<"b">>, D),
    ok = add(<<"c">>, D),
    ?assert(in(<<"a">>, D)),
    ?assert(in(<<"b">>, D)),
    ?assert(in(<<"c">>, D)),
    ?assertNot(in(<<"d">>, D)),
    ?assertNot(in(<<"e">>, D)),
    ?assertNot(in(<<"f">>, D)),
    ok = delete(<<"c">>, D),
    ?assertNot(in(<<"c">>, D)),
    ?assert(in(<<"a">>, D)),
    ?assert(in(<<"b">>, D)),
    ok = delete(<<"a">>, D),
    ?assertNot(in(<<"a">>, D)),
    ?assertNot(in(<<"c">>, D)),
    ?assert(in(<<"b">>, D)).

-ifdef(EQC).

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

qc(P) ->
    ?assert(eqc:quickcheck(?QC_OUT(P))).

basic_quickcheck_test_() ->
    Prop = eqc:numtests(5000, dlcbf:prop_add_are_members()),
    {timeout, 2*60, fun() -> qc(Prop) end}.

pos_int() ->
    ?LET(N, int(), abs(N) + 1).

power_of_two() ->
    ?LET(N, pos_int(), begin trunc(math:pow(2, N)) end).

prop_add_are_members() ->
    ?FORALL(M, non_empty(list(binary())),
            ?FORALL(N, non_empty(list(binary())),
                    ?LET({D, B}, {4, 2048},
                         ?IMPLIES(M -- N == M andalso
                                  M -- lists:usort(M) == [],
                                  check_membership(M, N, D, B))))).

check_membership(M, N, D, B) ->
    {ok, Dlcbf} = new(D, B),
    F = lists:foldl(fun(X, Acc) ->
                            add(X, Acc),
                            Acc
                    end, Dlcbf, M),
    lists:all(fun(X) ->
                      in(X, F)
              end, M) and
        lists:all(fun(X) ->
                          not in(X, F)
                  end, N).

keys() ->
    eqc_gen:non_empty(list(eqc_gen:non_empty(binary()))).

ops(Keys) ->
    {oneof([add, delete]), oneof(Keys)}.

apply_ops([], _Dlcbf, Acc) ->
    Acc;
apply_ops([{add, Key}|T], Dlcbf, Acc) ->
    ok = dlcbf:add(Key, Dlcbf),
    apply_ops(T, Dlcbf, orddict:store(Key, ok, Acc));
apply_ops([{delete, Key}|T], Dlcbf, Acc) ->
    ok = dlcbf:delete(Key, Dlcbf),
    apply_ops(T, Dlcbf, orddict:store(Key, deleted, Acc)).

prop_add_delete() ->
    ?LET(Keys, keys(),
         ?FORALL(Ops0, eqc_gen:non_empty(list(ops(Keys))),
                 begin
                     {ok, Dlcbf} = new(4, 2048),
                     Ops = lists:usort(Ops0),
                     Model = apply_ops(Ops, Dlcbf, []),
                     F = fun({Key, deleted}) ->
                                 ?assertEqual(false, dlcbf:in(Key, Dlcbf));
                            ({Key, ok}) ->
                                 ?assertEqual(true, dlcbf:in(Key, Dlcbf))
                         end,
                     lists:map(F, Model),
                     true
                 end)).

prop_add_delete_test_() ->
    Prop = eqc:numtests(5000, dlcbf:prop_add_delete()),
    {timeout, 2*60, fun() -> qc(Prop) end}.

-endif. % EQC

-endif.
