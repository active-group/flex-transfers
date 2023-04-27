-module(transfer_handler).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/0, stop/0, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_transfer/1]).

start_link() ->
    gen_server:start_link({local, transfer_handler}, ?MODULE, [], []).

stop() ->
    gen_server:stop(transfer_handler).

init([]) -> {ok, []}.

handle_cast(Msg, State) ->
    logger:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call({transact, Transfer}, _, State) ->
    SenderAccountNumber = Transfer#transfer.from_acc_nr,
    ReceiverAccountNumber = Transfer#transfer.to_acc_nr,
    Amount = Transfer#transfer.amount,
    MaybeAccSender = database:get_account(SenderAccountNumber),
    MaybeAccReceiver = database:get_account(ReceiverAccountNumber),

    Res =
        case {MaybeAccSender, MaybeAccReceiver} of
            {{error, account_not_found}, _} -> {error, sender_account_not_found};
            {_, {error, account_not_found}} -> {error, receiver_account_not_found};
            {{ok, _ , AccSenderAmount}, _} when AccSenderAmount - Amount >= 0 ->
                TxWithId = Transfer#transfer{id = database:next_transfer_id()},
                database:write_transfer(TxWithId),
                {ok, TxWithId#transfer.id};
            _ -> {error, insufficient_funds}
        end,

    {reply, Res, State};
handle_call(Msg, _, State) ->
    logger:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(Info, State) ->
    logger:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec handle_transfer(#transfer{}) -> {ok, unique_id()} | 
                                            {error, sender_account_not_found | 
                                                    receiver_account_not_found |
                                                    insufficient_funds}.
handle_transfer(Transfer) ->
    gen_server:call(transfer_handler, {transact, Transfer}).
