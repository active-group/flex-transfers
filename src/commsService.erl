-module(commsService).

-include("data.hrl").
-include("events.hrl").

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([start/0]).
-export([make_transaction/0]).
-export([sendEvent/2]).
-export([handle_events/1]).
-export([send_events/2]).

-behaviour(gen_server).

-type state() :: list(#get_transfer_events_since{}).

start() -> 
    {ok, PID} = gen_server:start(commsService, [], [{debug, [trace]}]),
    register(transfer_service, PID),
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
    logger:info("Adding request to state queue, ~s", [Message]),
    {noreply, [Message | State]}.

-spec handle_events(list(#get_transfer_events_since{})) -> ok.
handle_events(List) ->
    logger:info("Sending ~p", [List]),
    lists:foreach(fun(#get_transfer_events_since{since = Since, receiver_pid = PID}) ->
                    send_events(events:get_events_from(Since), PID)
                  end,
                  List),
    ok.

-spec send_events(list(#event{}), pid()) -> ok.
send_events(List, PID) ->
    logger:info("Sending ~p", [List]),
    lists:foreach(fun(Event) ->
                     sendEvent(Event, PID)
                  end,
                  List),
    ok.

-spec sendEvent(#event{}, pid()) -> ok.
sendEvent(#event{payload = EventMessage}, PID) ->
    logger:info("Event ~pto PID ~p", [EventMessage, PID]),
    PID ! EventMessage,
    ok.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

make_transaction() ->
    client:open_account(),
    client:open_account(),
    client:transfer(1, 2, 100),
    PID = start(),
    gen_server:cast(PID, #get_transfer_events_since{since = 0, receiver_pid = self()}),
    gen_server:cast(PID, send).
