%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, write/2, read_all/2,
  put_account/0, put_account/1, get_account/1, get_all_accounts/0,
  put_transfer/1, get_transfer/1, get_all_transfers/0, get_all_transfers/1, get_all_transfers_from/1,
  unique_account_number/0, unique_transfer_id/0,
  atomically/1, put_accounts/1, put_event_id/1, last_event_id/0]).

close_tables() ->
  dets:close(transfer),
  dets:close(account),
  dets:close(event_id),
  dets:close(table_id).

%% destroy tables in case they already existed
destroy_tables() ->
  file:delete("transfer.dets"),
  file:delete("account.dets"),
  file:delete("event_id.dets"),
  file:delete("table_id.dets").

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
  {ok, transfer} = dets:open_file(transfer, [{type, set}, {file, "transfer.dets"}]),
  {ok, account} = dets:open_file(account, [{type, set}, {file, "account.dets"}]),
  {ok, event_id} = dets:open_file(event_id, [{type, set}, {file, "event_id.dets"}]),
  {ok, table_id} = dets:open_file(table_id, [{type, set}, {file, "table_id.dets"}]),
  dets:insert(table_id, {transfer, 0}),
  dets:insert(table_id, {account, 0}),
  dets:insert(table_id, {event_id, 0}).

init_database() ->
  close_tables(),
  destroy_tables(),
  create_tables(),
  ok.

write(Table, Tuple) ->
  ok = dets:insert(Table, Tuple),
  ok.

-spec read_one(dets:tab_name(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
  Res = dets:lookup(Table, Id),
  case Res of
    [Tuple] -> {ok, Deserialize(Tuple)};
    [] -> {error, not_found};
    [_ | _] -> {error, more_than_one};
    Error -> Error
  end.

-spec read_all(dets:tab_name(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
  Res = dets:select(Table, [{'_', [], ['$_']}]),
  lists:map(Deserialize, Res).

-spec put_account() -> ok.
put_account() ->
  write(account, {1, 1000}),
  write(account, {2, 1000}).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber, amount = Amount}) ->
  write(account, {AccountNumber, Amount}).

-spec put_accounts([#account{}]) -> [ok].
put_accounts(Accounts) ->
  lists:map(fun put_account/1, Accounts).

-spec last_event_id() -> number().
last_event_id() ->
  case dets:lookup(table_id, event_id) of
    [] -> {error, no_event_id_found};
    [{event_id, EventId} | []] -> EventId;
    [_ | _] -> {error, multiple_id_entries_found};
    Error -> Error
  end.

-spec put_event_id(number()) -> ok.
put_event_id(Event_id) ->
  dets:insert(table_id, {event_id, Event_id}).

deserialize_account({AccountNumber, Amount}) ->
  #account{account_number = AccountNumber, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
  read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).

-spec put_transfer(#transfer{}) -> ok.
put_transfer(#transfer{id = Id, timestamp = Timestamp, from_account_number = FromAccountNumber, to_account_number = ToAccountNumber, amount = Amount}) ->
  write(transfer, {Id, Timestamp, FromAccountNumber, ToAccountNumber, Amount}).

deserialize_transfer({Id, Timestamp, FromAccountNumber, ToAccountNumber, Amount}) ->
  #transfer{id = Id, timestamp = Timestamp, from_account_number = FromAccountNumber, to_account_number = ToAccountNumber, amount = Amount}.

-spec get_transfer(unique_id()) -> {ok, #transfer{}} | {error, any()}.
get_transfer(Id) ->
  read_one(transfer, Id, fun deserialize_transfer/1).

-spec get_all_transfers() -> list(#transfer{}).
get_all_transfers() -> read_all(transfer, fun deserialize_transfer/1).

-spec get_all_transfers(account_number()) -> list(#transfer{}).
get_all_transfers(AccountNumber) ->
  Res = dets:select(transfer,
    [{'$1',
      [{'orelse',
        {'==', {element, 3, '$1'}, AccountNumber},
        {'==', {element, 4, '$1'}, AccountNumber}}],
      ['$_']}]),
  lists:map(fun deserialize_transfer/1, Res).

-spec get_all_transfers_from(unique_id()) -> list(#transfer{}).
get_all_transfers_from(Number) ->
  Res = dets:select(transfer,
    [{'$1',
      [{'>=', {element, 1, '$1'}, Number}],
      ['$_']}]),
  lists:map(fun deserialize_transfer/1, Res).

-spec unique_account_number() -> unique_id().
unique_account_number() -> dets:update_counter(table_id, account, 1).

-spec unique_transfer_id() -> unique_id().
unique_transfer_id() -> dets:update_counter(table_id, transfer, 1).

% holdover from Mnesia
-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Function) ->
  Function().
