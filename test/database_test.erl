-module(database_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/data.hrl").

setup() ->
  database:init_database().

cleanup(_) -> ok.

main_test_() ->
  {inorder,
    {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun put_transfer/1,
        fun last_event_id/1]
    }}.


put_transfer(_) ->
  fun() ->
    Transfer = #transfer{id = 17, timestamp = {1610, 547469, 326863}, from_account_number = 17, to_account_number = 32, amount = 100},
    database:put_transfer(Transfer),
    ?assertEqual(database:get_transfer(17), {ok, Transfer}),
    ?assertEqual(database:get_all_transfers(), [Transfer]),
    ?assertEqual(database:get_all_transfers(17), [Transfer]),
    ?assertEqual(database:get_all_transfers(16), [])
  end.

last_event_id(_) ->
  fun() ->
    ?assertEqual(database:last_event_id(), 0),
    database:put_event_id(5),
    ?assertEqual(database:last_event_id(), 5)
  end.





