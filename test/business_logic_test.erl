-module(business_logic_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    transfer_handler:start_link(),
    database:init_database(),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {0, 1000}}),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {1, 1000}}),
    timer:sleep(200).

cleanup(_) ->
    transfer_handler:stop(),
    database:close_tables(),
    database:destroy_tables().

main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun make_a_valid_tx/1,
       fun overspending_is_permitted/1,
       fun balance_is_updated/1,
       fun aborts_when_sender_not_found/1,
       fun aborts_when_receiver_not_found/1
      ]}}.



make_a_valid_tx(_) ->
    fun() ->
            Tx = business_logic:transfer(0, 1, 1000),
            ?assertEqual({ok, 1}, Tx)
    end.

overspending_is_permitted(_) ->
    fun() ->
            Tx = business_logic:transfer(0, 1, 1001),
            ?assertEqual(Tx, {error, insufficient_funds}),
            Events = database:transfer_events(),
            ?assertEqual(length(Events), 0)
    end.

balance_is_updated(_) ->
    fun() ->
            Tx = business_logic:transfer(0, 1, 1000),
            ?assertEqual(Tx, {ok, 1}),

            Tx1 = business_logic:transfer(0, 1, 1000),
            ?assertEqual(Tx1, {error, insufficient_funds}),

            Tx2 = business_logic:transfer(1, 0, 1500),
            ?assertEqual(Tx2, {ok, 2}),

            Tx3 = business_logic:transfer(1, 0, 1501),
            ?assertEqual(Tx3, {error, insufficient_funds})
    end.

aborts_when_sender_not_found(_) ->
    fun() ->
            Tx = business_logic:transfer(3, 1, 1000),
            ?assertEqual(Tx, {error, sender_account_not_found}),
                Events = database:transfer_events(),
            ?assertEqual(length(Events), 0)
    end.

aborts_when_receiver_not_found(_) ->
    fun() ->
            Tx = business_logic:transfer(0, 3, 1000),
            ?assertEqual(Tx, {error, receiver_account_not_found}),
            Events = database:transfer_events(),
            ?assertEqual(length(Events), 0)
    end.
