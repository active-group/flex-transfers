-module(events).
-export([init_events/0, put_event/1, get_all_events/0, get_events_from/1, delete_transaction_succeeded_event/1]).
-include("events.hrl").
-include("data.hrl").

% call after database:init_database/0

init_events() ->
  dets:close(event),
  file:delete("event.dets"),
  {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
  dets:insert(table_id, {event, 0}).


-spec unique_event_number() -> non_neg_integer().
unique_event_number() -> dets:update_counter(table_id, event, 1).

-spec put_event(term()) -> #event{}.
put_event(Payload) ->
  Number = unique_event_number(),
  database:write(event, {Number, Payload}),
  #event{number = Number, payload = Payload}.

-spec delete_transaction_succeeded_event(unique_id()) -> none().
delete_transaction_succeeded_event(TransactionId) ->
  Pred = fun(#event{payload = Payload}) -> Payload#transaction_succeeded.transaction_id == TransactionId end,
  case lists:filter(Pred, get_all_events()) of
    [TransactionSucceededEvent | _] -> database:delete(event, TransactionSucceededEvent#event.number);
    [] -> noop
  end.


-spec deserialize_event({non_neg_integer(), term()}) -> #event{}.
deserialize_event({Number, Payload}) ->
  #event{number = Number, payload = Payload}.

-spec get_all_events() -> [#event{}].
get_all_events() ->
  database:read_all(event, fun deserialize_event/1).

-spec get_events_from(non_neg_integer()) -> [#event{}].
get_events_from(Number) ->
  Res = dets:select(event,
    [{'$1',
      [{'>=', {element, 1, '$1'}, Number}],
      ['$_']}]),
  Events = lists:map(fun deserialize_event/1, Res),
  lists:sort(fun(#event{number = Number1}, #event{number = Number2}) -> Number1 =< Number2 end, Events).
