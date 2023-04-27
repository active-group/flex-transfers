-module(accounts_mock).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/2, start_demo_link/0]).

start_demo_link() ->
    start_link([{event, 0, {person, 1, <<"Mike">>, <<"Sperber">>}},
                 {event, 1, {account, 15, 1, 1500}},
                 {event, 2, {person, 2, <<"Simon">>, <<"Härer>">>}},
                 {event, 3, {account, 16, 2, 1600}}],
                [{event, 4, {person, 3, <<"Mike">>, <<"Sperber">>}},
                 {event, 5, {account, 17, 3, 1700}},
                 {event, 6, {person, 4, <<"Simon">>, <<"Härer>">>}},
                 {event, 7, {account, 18, 4, 1800}}]).

start_link(InitialEvents, LaterEvents) ->
    gen_server:start_link({local, account_service}, ?MODULE, {InitialEvents, LaterEvents}, []).

init(State) -> {ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({Pid, no_events}, _From, {InitialEvents, LaterEvents}) ->
    lists:foreach(fun (Event) -> gen_server:cast(Pid, Event) end, LaterEvents),
    {reply, {ok, InitialEvents}, {[], LaterEvents}};

handle_call({Pid, {event, LastEventNumber, _}}, _From, {InitialEvents, LaterEvents}) ->
    Events = lists:filter(fun ({event, EventNumber, _}) -> EventNumber > LastEventNumber end, 
                                  InitialEvents),
    lists:foreach(fun (Event) -> gen_server:cast(Pid, Event) end, LaterEvents),
    {reply, {ok, Events}, {[], []}}.
