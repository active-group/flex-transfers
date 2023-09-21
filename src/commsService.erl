-module(commsService).

-include("data.hrl").

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([start/0]).

-behaviour(gen_server).

-type state() :: list(#get_transfer_events_since{}).

start() -> gen_server:start(sender, [], [{debug, [trace]}]).

-spec init(list(#get_transfer_events_since{})) -> {ok, state()}.
init(State) ->
    {ok, State}.

-spec handle_cast(#get_transfer_events_since{} | send, state()) -> {noreply, state()}.
handle_cast(send, State) ->
    send_events(State),
    {noreply, []};
handle_cast(#get_transfer_events_since{} = Message, State) ->
    {noreply, [Message | State]}.

-spec send_events(list(#get_transfer_events_since{})) -> ok.
send_events(List) ->
    lists:foreach(fun({since = Since, receiver_pid = PID}) ->
                     sendEvent(events:get_events_from(Since), PID)
                  end,
                  List),
    ok.

-spec sendEvent(#transfer_event{}, pid()) -> ok.
sendEvent(#transfer_event{} = Event, PID) ->
    PID ! Event,
    ok.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.
