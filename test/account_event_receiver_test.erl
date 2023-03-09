-module(account_event_receiver_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/data.hrl").

cleanup(_) -> ok.

main_test_() ->
  {inorder,
    {foreach,
      fun cleanup/1,
      [fun filter_for_new_account_events/1]
    }}.


filter_for_new_account_events(_) ->
  fun() ->
    Empty_events = [],
    ?assertEqual(account_event_receiver:filter_for_new_account_events(Empty_events), []),
    Events = [{event, 1, new_person_event, {1, <<"Franz">>, <<"Huber">>}}, {event, 2, new_account_event, {1, 1, 1000}}],
    ?assertEqual(account_event_receiver:filter_for_new_account_events(Events), [{1, 1, 1000}])
  end.
