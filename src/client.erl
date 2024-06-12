%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([make_account/1, transfer/3]).


%% opens an acocunt with a given name and surname.
%% prints the result and the account number to stdout.
-spec make_account(account_number()) -> ok.
make_account(AccountNumber) ->
    Account = business_logic:make_account(AccountNumber),
    io:format("Account was successfully opened. Account number: ~p ~n", [Account#account.account_number]).


%% transfers a given amount from the first account to the second account, identified
%% by their account number. Prints the transfer-id when successful, else the error
%% to stdout.
-spec transfer(account_number(), account_number(), money()) -> ok.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->
    case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
        {ok, TransferId} ->
            io:format("Transfer successful, id: ~p~n", [TransferId]);
        {error, Error} ->
            io:format("An error occured: ~p~n", [Error])
        end.


