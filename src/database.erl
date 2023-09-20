%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, write/2, read_all/2,
         put_transfer/1, unique_transfer_id/0,
         atomically/1]).

close_tables() ->
    dets:close(transfer),
    dets:close(table_id).

%% destroy tables in case they already existed
destroy_tables() ->
    file:delete("transfer.dets").

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
    {ok, transfer} = dets:open_file(transfer, [{type, set}, {file, "transfer.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{type, set}, {file, "table_id.dets"}]),
    dets:insert(table_id, {transfer, 0}).

init_database() ->
    close_tables(),
    destroy_tables(),
    create_tables(),
    ok.

write(Table, Tuple) ->
    ok = dets:insert(Table, Tuple),
    ok.

-spec read_all(dets:tab_name(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Res = dets:select(Table,[{'_',[],['$_']}]),    
    lists:map(Deserialize, Res).

-spec put_transfer(#transfer{}) -> ok.
put_transfer(#transfer{id = Id, timestamp = Timestamp, from_account_number = FromAccountNumber, to_account_number = ToAccountNumber, amount = Amount}) ->
    write(transfer, {Id, Timestamp, FromAccountNumber, ToAccountNumber, Amount}).

-spec unique_transfer_id() -> unique_id().
unique_transfer_id() -> dets:update_counter(table_id, transfer, 1).

% holdover from Mnesia
-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Function) ->
    Function().
