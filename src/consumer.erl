-module(consumer).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/1, handle_cast/2, handle_info/2, process_events/2, terminate/2, code_change/3,
         make_state/2, update_pointer/2]).

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

-spec persist_event(#event{}) -> ok | nothing.
persist_event(#event{type = new_account_event, content = {AccountNumber, _, Amount}}) ->
    logger:info("Persisting account with number: ~p~n", [AccountNumber]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {AccountNumber, Amount}});

persist_event(#event{type = new_person_event}) -> nothing.

-spec next_index(list(#event{}), non_neg_integer()) -> non_neg_integer().
next_index([], Pointer) -> Pointer;
next_index(Events, _) -> lists:max(lists:map(fun(Event) -> Event#event.index end, Events)) + 1.

-spec update_data(list(#event{})) -> ok.
update_data(Events) ->
    lists:foreach(fun persist_event/1, Events).

process_events([], State) -> State;
process_events(Events, State) ->
    Pointer = State#state.pointer,
    %% A former (seemingly timed out) msg can come back. We dont want to
    %% double store events.
    E = lists:filter(fun(Ev) -> Ev#event.index >= Pointer end, Events),
    update_data(E),
    State#state{pointer = next_index(E, Pointer)}.

start_link(AccountsNode) ->
    gen_server:start_link({local, accounts_consumer}, ?MODULE, [AccountsNode], [{debug, [trace]}]).

init([AccountsNode]) ->
    % timer:send_interval(10000, fetch),
    self() ! fetch,
    {ok, #state{accounts_node = AccountsNode,
                pointer = 0}}.

handle_cast({person, _Id, _GivenName, _Surname}, State) ->
    {noreply, State};

handle_cast({account, AccountNumber, _PersonId, Amount}, State) ->
    logger:info("Persisting account with number: ~p~n", [AccountNumber]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {AccountNumber, Amount}}),
    {noreply, State};

handle_cast(Msg, State) ->
    logger:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _, State) ->
    logger:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(fetch, State) ->
    try
        AccountsNode = State#state.accounts_node,
        % Pointer = State#state.pointer,
        ok = gen_server:call({account_node, AccountsNode}, self()),
        {noreply, State}
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
