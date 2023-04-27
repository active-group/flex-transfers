-module(consumer_test).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:close_tables(),
    database:destroy_tables(),
    database:init_database().

cleanup(_) ->
    database:close_tables(),
    database:destroy_tables().


main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun events_are_stored/1,
      fun same_events_are_only_stored_once/1,
      fun keeps_state_if_timeout/1
     ]}.



events_are_stored(_) ->
    fun() ->
            State = consumer:make_state(node_node, 0),

            State1 = consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State),

            ?assertEqual(State1, consumer:update_pointer(State, 1)),

            Events = database:account_events(),
            ?assertEqual(1, length(Events)),

            % FIXME: State or State1
            State2 = consumer:process_events([{event, 1, new_account_event, {0, something, 1000}}], State),

            ?assertEqual(State2, consumer:update_pointer(State, 2)),

            Events2 = database:account_events(),
            ?assertEqual(2, length(Events2))
    end.


same_events_are_only_stored_once(_) ->
    fun() ->
            State = consumer:make_state(no_node, 0),

            State1 = consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State),

            ?assertEqual(State1, consumer:update_pointer(State, 1)),

            Events = database:account_events(),
            ?assertEqual(1, length(Events)),

            State2 = consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State1),

            ?assertEqual(State2, consumer:update_pointer(State, 1)),

            Events2 = database:account_events(),

            ?assertEqual(1, length(Events2))
    end.

keeps_state_if_timeout(_) ->
    fun() ->
            State = #{accounts_node => no_node,
                      pointer => 0},

            {noreply, State1} = consumer:handle_info(fetch, State),

            ?assertEqual(State, State1)
    end.
