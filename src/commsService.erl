-module(commsService).

-include("data.hrl").
-include("events.hrl").

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([start/0]).
-export([make_transactions/0]).
-export([sendEvent/2]).
-export([handle_events/1]).
-export([send_events/2]).

-behaviour(gen_server).

-type state() :: list(#get_transfer_events_since{}).

start() -> 
    {ok, PID} = gen_server:start(commsService, [], [{debug, [trace]}]),
    register(transfers, PID),
    %make_transactions(),
    PID.

-spec init(list(#get_transfer_events_since{})) -> {ok, state()}.
init(State) ->
    {ok, State}.

-spec handle_cast(#get_transfer_events_since{} | send, state()) -> {noreply, state()}.
handle_cast(send, State) ->
    logger:info("Request to send logs received."),
    handle_events(State),
    {noreply, []};
handle_cast(#get_transfer_events_since{} = Message, State) ->
    logger:info("Adding request to state queue, ~p", [Message]),
    handle_events([Message]),
    {noreply, []}.

-spec handle_events(list(#get_transfer_events_since{})) -> ok.
handle_events(List) ->
    logger:info("Sending ~p", [List]),
    lists:foreach(fun(#get_transfer_events_since{since = Since, receiver_pid = PID}) ->
                    Events_from_number = events:get_events_from(Since),
                    Transfer_events = get_transfer_events(Events_from_number),
                    send_events(Transfer_events, PID)
                  end,
                  List),
    ok.

-spec get_transfer_events(list(#event{})) -> list(#event{}).
get_transfer_events(Events) ->
    logger:info("Filtering Events for transfer_events"),
    lists:filter(fun(#event{payload = Event}) -> is_transfer_event(Event) end, Events).

-spec is_transfer_event(term()) -> boolean().
is_transfer_event(#internal_transfer_event{}) -> true;
is_transfer_event(_) -> false.


-spec send_events(list(#event{}), pid()) -> ok.
send_events(List, PID) ->
    logger:info("Sending ~p", [List]),
    lists:foreach(fun(Event) ->
                     sendEvent(Event, PID)
                  end,
                  List),
    ok.



-spec sendEvent(#event{}, pid()) -> ok.
sendEvent(#event{number = EventNumber, payload = EventMessage}, PID) ->
    EventToSend = #transfer_event{
        source = transfer_service,
        eventId = EventNumber,
        accountIdSender = EventMessage#internal_transfer_event.accountIdSender,
        accountIdReceiver = EventMessage#internal_transfer_event.accountIdReceiver,
        amount = EventMessage#internal_transfer_event.amount,
        timestamp = EventMessage#internal_transfer_event.timestamp
    },
    logger:info("Event ~p to PID ~p", [EventToSend, PID]),
    gen_server:cast(PID, EventToSend),
    ok.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

make_transactions() ->
    client:open_account(),
    client:open_account(),
    client:transfer(1, 2, 100),
    client:transfer(1, 2, 100),
    client:transfer(1, 2, 100),
    client:transfer(1, 2, 100),
    client:transfer(1, 2, 100),
    client:transfer(1, 2, 100),
    client:transfer(2, 1, 10),
    client:transfer(2, 1, 10),
    client:transfer(2, 1, 10),
    client:transfer(2, 1, 10).