%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([transfer/3]).

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