%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([transfer/3]).

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when there is not enough money in the sender account.

-spec transfer(account_number(), account_number(), money()) -> 
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->

    TransferFunction =
      fun() -> 

        MaybeAccountSender = account_connector:get_account(SenderAccountNumber),
        MaybeAccountReceiver = account_connector:get_account(ReceiverAccountNumber),
        case {MaybeAccountSender, MaybeAccountReceiver} of
            {{error, not_found}, _} -> {error, sender_account_not_found};
            {_, {error, not_found}} -> {error, receiver_account_not_found};

            {{ok, AccountSender}, {ok, AccountReceiver}} ->
                AccountSenderAmount = AccountSender#account.amount,
                AccountReceiverAmount = AccountReceiver#account.amount,

                if
                    AccountSenderAmount - Amount >= 0 ->
                        TransferId = database:unique_transfer_id(),
                        Transfer = #transfer{id = TransferId,
                                            timestamp = erlang:timestamp(),
                                            from_account_number = SenderAccountNumber,
                                            to_account_number = ReceiverAccountNumber,
                                            amount = Amount},
                        NewAccountReceiver = AccountReceiver#account{amount = (AccountReceiverAmount + Amount)},
                        NewAccountSender = AccountSender#account{amount = (AccountSenderAmount - Amount)},
                        database:put_transfer(Transfer),
                        account_connector:put_account(NewAccountSender),
                        account_connector:put_account(NewAccountReceiver),
                        {ok, TransferId};
                    true ->
                        {error, insufficient_funds}
                end
        end
      end,

    database:atomically(TransferFunction).
