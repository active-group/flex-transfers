-module(receiveService).

-include("data.hrl").
-include("events.hrl").

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([start/1]).

-behaviour(gen_server).

-type state() :: { number(),  node()}.

start(AccountNode) -> 
    {ok, PID} = gen_server:start(receiveService, {0, AccountNode}, [{debug, [trace]}]),
    register(transfers_accounts, PID),
    PID.

-spec init({ number(), node()}) -> {ok, state()}.
init({LastKnownEvent, Node}) ->
    Events = events:get_all_events(),
    case Events of
       [] -> logger:info("sending request to account service"),
             gen_server:cast({accounts, Node}, #get_account_events_since{since = 0, receiver_pid =self()}),
             {ok, {0, Node}};
       _ -> #event{number = LastKnownEventFromDB} = lists:last(Events),
             logger:info("sending request to account service"),
             gen_server:cast({accounts, Node}, #get_account_events_since{since = LastKnownEventFromDB, receiver_pid =self()}),
             {ok, {LastKnownEventFromDB, Node}}            
    end.

-spec handle_cast(#account_event{}, state()) -> {noreply, state()}.
handle_cast(#account_event{account_number = AccountNumber} = Message, _State) ->
    logger:info("processing event, ~p", Message),
    #event{number = LastKnownEvent} = events:put_event(Message),
    #account{account_number = AccountNumber} = business_logic:open_account_with_account_number(AccountNumber),
    events:put_event(#transfer_event{
                                        source = transfer_service,
                                        accountIdSender = 0,
                                        accountIdReceiver = AccountNumber,
                                        amount = 1000,
                                        timestamp = erlang:timestamp()}),
    %message verarbeiten
    {noreply, LastKnownEvent}.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.


