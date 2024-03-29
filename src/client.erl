%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([transfer/3]).
-export([open_account/0]).
-export([print_transfer/1]).
-export([print_transfers/1]).


%% opens an acocunt with a given name and surname.
%% prints the result and the account number to stdout.
-spec open_account() -> ok.
open_account() ->
    Account = business_logic:open_account(),
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


%% takes a transfer record and prints it to stdout.
print_transfer(Transfer) ->
    AccountNumber1 = Transfer#transfer.from_account_number,
    AccountNumber2 = Transfer#transfer.to_account_number,
    Amount = Transfer#transfer.amount,
    Id = Transfer#transfer.id,
    io:format("#~p\t ~p\t ~s \t -> ~s ~n", [Id, Amount, AccountNumber1, AccountNumber2]).

%% takes a list of transfers records and prints them to stdout
print_transfers(Transfers) ->
    lists:map(fun print_transfer/1, Transfers).
