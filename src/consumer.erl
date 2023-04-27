-module(consumer).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/1, handle_cast/2, handle_info/2, process_events/2, terminate/2, code_change/3,
         make_state/2, update_pointer/2]).

-type account_event() :: {event, non_neg_integer(), term()}.

% FIXME: move to data.hrl?
-record(state, {accounts_node :: node(), pointer :: non_neg_integer()}).

%% for testing
-spec make_state(node(), non_neg_integer()) -> #state{}.
make_state(AccountsNode, Pointer) ->
    #state{accounts_node = AccountsNode,
           pointer = Pointer}.

-spec update_pointer(#state{}, non_neg_integer()) -> #state{}.
update_pointer(State, Pointer) ->
    State#state{ pointer = Pointer}.

-spec persist_event(account_event()) -> ok | nothing.
persist_event({event, _Number, {account, AccountNumber, _PersonId, Amount}}) ->
    logger:info("Persisting account with number: ~p~n", [AccountNumber]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {AccountNumber, Amount}});

persist_event(_Event) -> nothing.

-spec next_index(list(account_event()), non_neg_integer()) -> non_neg_integer().
next_index([], Pointer) -> Pointer;
next_index(Events, _) -> lists:max(lists:map(fun({event, Number, _Payload}) -> Number end, Events)).

-spec update_data(list(account_event())) -> ok.
update_data(Events) ->
    lists:foreach(fun persist_event/1, Events).

-spec process_events(list(account_event()), #state{}) -> #state{}.
process_events([], State) -> State;
process_events(Events, State) ->
    Pointer = State#state.pointer,
    %% A former (seemingly timed out) msg can come back. We dont want to
    %% double store events.
    E = case Pointer of 
            no_events -> Events;
            _ -> lists:filter(fun({event, Number, _Payload}) -> Number > Pointer end, Events)
        end,
    update_data(E),
    State#state{pointer = next_index(E, Pointer)}.

start_link(AccountsNode) ->
    gen_server:start_link({local, accounts_consumer}, ?MODULE, AccountsNode, [{debug, [trace]}]).

init(AccountsNode) ->
    timer:send_interval(10000, fetch),
    self() ! fetch,
    {ok, #state{accounts_node = AccountsNode,
                pointer = no_events}}.

handle_cast(Msg, State) ->
    logger:info("Ignoring cast message: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _, State) ->
    logger:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(fetch, State) ->
    try
        AccountsNode = State#state.accounts_node,
        Pointer = State#state.pointer,
        Msg = case Pointer of
                  no_events -> no_events;
                  Number -> {event, Number, dummy_payload}
              end,
        {ok, Events} = gen_server:call({account_service, AccountsNode}, {self(), Msg}),
        NewState = process_events(Events, State),
        {noreply, NewState}
    catch
        Error:Reason ->
            logger:error("Error consuming account events: ~p: ~p", [Error, Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    logger:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
