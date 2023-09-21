-module(events).
-export([init_events/0, put_event/1, get_all_events/0, get_events_from/1, get_events_from_with/2,
         get_transfer_events/1]).
-include("events.hrl").
-include("data.hrl").

% call after database: init_database/0

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
    numberedPayload = Payload#transfer_event{eventId = Number},
    database:write(event, {Number, Payload}),
    #event{number = Number, payload = Payload}.

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
    lists:sort(fun (#event{number = Number1}, #event{number = Number2}) -> Number1 =< Number2 end, Events).

-spec get_events_from_with(non_neg_integer(), any()) -> [#event{}].
get_events_from_with(Number, MatchFun) ->
    Res = dets:select(event,
                        [{'$1',
                        [{'andalso', {'>=', {element, 1, '$1'}, Number}, MatchFun({element, 2, '$1'})}],
                        ['$_']}]),
    Events = lists:map(fun deserialize_event/1, Res),
    lists:sort(fun (#event{number = Number1}, #event{number = Number2}) -> Number1 =< Number2 end, Events).

-spec get_transfer_events(non_neg_integer()) -> [#event{}].
get_transfer_events(From) ->
    get_events_from_with(From, fun (Payload) -> {'==', {element, 1, Payload}, transfer_event} end).
