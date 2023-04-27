-module(feed_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:init_database(),
    database:put_event(
      #event{index = 0,
             type = new_transfer_event,
             content = {0, os:timestamp(), 0, 1, 1}}),
    database:put_event(
      #event{index = 1,
             type = new_transfer_event,
             content = {1, os:timestamp(), 0, 1, 1}}).


cleanup(_) ->
    database:close_tables(),
    database:destroy_tables().


main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun consume_tx_from_beginning/1,
       fun consume_tx_from_offset/1,
       fun consume_no_tx/1
      ]}}.


consume_tx_from_beginning(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 0}, bla, []),
            ?assertEqual(length(Events), 2)
    end.


consume_tx_from_offset(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 1}, bla, []),
            ?assertEqual(length(Events), 1)
    end.


consume_no_tx(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 5}, bla, []),
            ?assertEqual(length(Events), 0)
    end.
