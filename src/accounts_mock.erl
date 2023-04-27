-module(accounts_mock).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/1, start_demo_link/0]).

start_demo_link() ->
    start_link([{event, 0, {person, 1, <<"Mike">>, <<"Sperber">>}},
                {event, 1, {account, 15, 1, 1500}},
                {event, 2, {person, 2, <<"Simon">>, <<"Härer>">>}},
                {event, 3, {account, 16, 2, 1600}}]).

start_link(Events) ->
    gen_server:start_link({local, account_service}, ?MODULE, Events, []).

init(Events) -> {ok, Events}.

handle_cast(_Msg, Events) -> {noreply, Events}.

handle_call({_Pid, {event, LastEventNumber, _Payload}}, _From, Events) ->
    Events = case LastEventNumber of
                 no_events -> Events;
                 _ ->
                     lists:filter(fun ({event, EventNumber, _Payload}) -> EventNumber > LastEventNumber end, 
                                  Events)
             end,
    {reply, {ok, Events}, Events}.
                          
                          

