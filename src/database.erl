-module(database).
-include("data.hrl").
-export([next_event_index/0, put_event/1, transfer_events/0,
         get_account_transfers/1, get_account/1,
         transfer_events_from/1,
         write_transfer/1,
         account_events/0, init_database/0, close_tables/0, destroy_tables/0, next_transfer_id/0]).

close_tables() ->
    dets:close(event),
    dets:close(table_id).

destroy_tables() ->
    file:delete("event.dets"),
    file:delete("table_id.dets").

create_tables() ->
    {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{file, "table_id.dets"}]),
    ok = dets:insert(table_id, {event, 0}),
    ok = dets:insert(table_id, {transfer, 0}).

init_database() ->
    close_tables(),
    destroy_tables(),
    create_tables(),
    ok.

write(Table, Tuple) ->
    dets:insert(Table, Tuple).

-spec put_event(#event{}) -> ok.
put_event(#event{index = Index, type = Type, content = Content}) ->
    write(event, {Index, Type, Content}).

deserialize_event({Index, Type, Content}) ->
    #event{index = Index, type = Type, content = Content}.

-spec get_all_events(atom()) -> list(#event{}).
get_all_events(Type) ->
    Res = dets:select(event, [{'$1', [{'==', {element, 2, '$1'}, Type}], ['$_']}]),
    lists:map(fun deserialize_event/1, Res).

-spec account_events() -> list(#event{}).
account_events() ->
    get_all_events(new_account_event).

-spec account_events(account_number()) -> list(#event{}).
account_events(AccountNr) ->
    Res = dets:select(event, [{'$1', [{'andalso',
                                        {'==', {element, 2, '$1'}, new_account_event},
                                        {'==', {element, 1, {element, 3, '$1'}}, AccountNr}}], ['$_']}]),
    lists:map(fun deserialize_event/1, Res).

-spec transfer_events() -> list(#event{}).
transfer_events() ->
    get_all_events(new_transfer_event).

-spec transfer_events_from(non_neg_integer()) -> list(#event{}).
transfer_events_from(Index) ->
    Res = dets:select(event, [{'$1',
                             [{'andalso',
                                {'==', {element, 2, '$1'}, new_transfer_event},
                                {'>=', {element, 1, '$1'}, Index}}],
                             ['$_']}]),
    lists:map(fun deserialize_event/1, Res).

-spec squash_transfer({unique_id(), erlang:timestamp(), account_number(), account_number(), money()},
                         {account_number(), money()}) 
        ->  {account_number(), money()}.
squash_transfer({_, _, Sender, _, TxAmount}, {AccountNumber, Amount}) ->
    if (Sender == AccountNumber) ->
            {AccountNumber, Amount - TxAmount};
       true -> {AccountNumber, Amount + TxAmount}
    end.

-spec get_account_created(account_number()) -> list(#event{}).
get_account_created(AccountNumber) ->
    account_events(AccountNumber).

-spec get_account(account_number()) -> {ok, account_number(), money()} | {error, account_not_found}.
get_account(AccountNumber) ->
    MaybeAcc = get_account_created(AccountNumber),
    case MaybeAcc of
        [{event, _, _, Acc}] ->
            TransfersEvents = get_account_transfers(AccountNumber),
            Transfers = lists:map(fun (Event) -> Event#event.content end, TransfersEvents),
            {Account, Amount} = lists:foldl(fun squash_transfer/2, Acc, Transfers),
            {ok, Account, Amount};
        _ -> {error, account_not_found}
    end.

-spec get_account_transfers(account_number()) -> list(#event{}).
get_account_transfers(AccountNumber) ->
    TransferEvents = transfer_events(),
    MatchingTransfers = lists:filter(fun(#event{content = {_, _, Sender, Receiver, _}}) ->
                                                (Sender == AccountNumber) orelse
                                                (Receiver == AccountNumber) end,
                                        TransferEvents),
    %% We can sort here by Event or TxId, doesn't matter
    lists:sort(fun ({_, ID1, _, _}, {_, ID2, _, _}) -> ID1 > ID2 end, MatchingTransfers).

-spec write_transfer(#transfer{}) -> ok.
write_transfer(Tx) ->
    Event = #event{index = next_event_index(),
                   type = new_transfer_event,
                   content = {Tx#transfer.id,
                              Tx#transfer.timestamp,
                              Tx#transfer.from_acc_nr,
                              Tx#transfer.to_acc_nr, Tx#transfer.amount}},
    put_event(Event).

-spec next_event_index() -> non_neg_integer().
next_event_index()-> dets:update_counter(table_id, event, 1).

-spec next_transfer_id() -> unique_id().
next_transfer_id()-> dets:update_counter(table_id, transfer, 1).
