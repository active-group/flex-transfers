-module(event_server).
-export([init/1, start/0, handle_cast/2, handle_call/3]).
-include("event.hrl").

-behavior(gen_server).

% state of gen_server
-type registered_pids() :: sets:set(pid()).
-type event() :: #person_creation_event{} | #account_creation_event{} | #transfer_creation_event{}.

start() ->
  gen_server:start(?MODULE, sets:new(), [{debug, [trace]}]).

-spec init(registered_pids()) -> {ok, registered_pids()}.
init(Pids) ->
  {ok, Pids}.

-spec handle_call(#register{}, pid(), registered_pids()) -> {reply, any(), registered_pids()}.
handle_call(#register{}, {Pid, _}, Registered_Pids) ->
  New_Pids = sets:add_element(Pid, Registered_Pids),
  {reply, ok, New_Pids}.

-spec send_event(event(), pid()) -> {any()}.
send_event(Event, Pid) ->
  call(Pid, Event).

call(Pid, Request) ->
  io:format("Pid: ~p, Request: ~p~n", [Pid, Request]),
  spawn(fun() -> call_loop(Pid, Request) end).

call_loop(Pid, Request) ->
  try
    case gen_server:call(Pid, Request) of
      ok -> ok;
      _ ->
        timer:sleep(1000),
        call_loop(Pid, Request)
    end
  catch
    exit:{noproc,_} ->
      timer:sleep(1000),
      call_loop(Pid, Request)
  end.

% wird aus der business_logic aufgerufen
-spec handle_cast(event(), registered_pids()) -> {noreply, registered_pids()}.
handle_cast(Event, Pids) ->
  send_event_to_pids(Event, sets:to_list(Pids)), {noreply, Pids}.
  %durch alle Pids gehen
  % event schicken


-spec send_event_to_pids(event(), list(pid())) -> {any()}.
send_event_to_pids(Event, []) -> ok;
send_event_to_pids(Event, [First | Rest]) ->
  send_event(Event, First),
  send_event_to_pids(Event, Rest).