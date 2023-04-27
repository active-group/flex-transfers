-module(client).
-include("data.hrl").
-export([start/0, transfer/3]).



%% transfers a given amount from the first account to the second account, identified
%% by their account number. Prints the transfer-id when successful, else the error
%% to stdout.
-spec transfer(account_number(), account_number(), money()) -> ok.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->
    case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
        {ok, TxId} ->
            io:format("Transfer successful, id: ~p~n", [TxId]);
        {error, Err} ->
            io:format("An error occured: ~p~n", [Err])
    end.


start() ->
	ok.
