-module(lists2).

-export([ukeysublist/3]).

-spec ukeysublist(N, TupleList1, TupleList2) ->
    TupleList3 when
    N :: non_neg_integer(),
    TupleList2 :: TupleList1,
    TupleList3 :: TupleList1,
    TupleList1 :: [tuple()].

ukeysublist(N, [H1|T1]=L1, [H2|T2]=L2) ->
    E1 = element(N, H1),
    E2 = element(N, H2),
    if E1 < E2 -> [H1|ukeysublist(N, T1, L2)];
       E1 > E2 -> ukeysublist(N, L1, T2);
       true    -> ukeysublist(N, T1, T2)
    end;
ukeysublist(_N, L1, _L2) ->
    L1.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ukeysublist_test_() ->
    [?_assertEqual(ukeysublist(1, [{1}, {2}, {3}], [{2}]),
                   [{1}, {3}])].

-endif.

