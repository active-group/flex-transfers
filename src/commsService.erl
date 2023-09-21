-module(commsService).

-include("data.hrl").

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([start/0]).
-export([make_transaction/0]).

-behaviour(gen_server).

-type state() :: list(#get_transfer_events_since{}).

start() -> 
    {ok, PID} = gen_server:start(commsService, [], [{debug, [trace]}]),
    PID.

-spec init(list(#get_transfer_events_since{})) -> {ok, state()}.
init(State) ->
    {ok, State}.

-spec handle_cast(#get_transfer_events_since{} | send, state()) -> {noreply, state()}.
handle_cast(send, State) ->
    logger:info("Request to send logs received."),
    send_events(State),
    {noreply, []};
handle_cast(#get_transfer_events_since{} = Message, State) ->
    logger:info("Adding request to state queue, ~s", [Message]),
    {noreply, [Message | State]}.

-spec send_events(list(#get_transfer_events_since{})) -> ok.
send_events(List) ->
    logger:info("Sending ~p", [List]),
    lists:foreach(fun({since = Since, receiver_pid = PID}) ->
                     sendEvent(events:get_events_from(Since), PID)
                  end,
                  List),
    ok.

-spec sendEvent(#transfer_event{}, pid()) -> ok.
sendEvent(#transfer_event{} = Event, PID) ->
    logger:info("Event ~s to PID ~s", [Event, PID]),
    PID ! Event,
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
