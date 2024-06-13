%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, transfer/3, make_account/2]).


%% Opens an account, that is creates a new account containing a new person 
%% Writes them into database.


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).

-spec make_account(account_number(), money()) -> #account{}.
make_account(AccountNumber, InitialAmount) ->
  Account = #account{account_number = AccountNumber,
    amount = InitialAmount},
  case database:get_account(AccountNumber) of
    {ok, Account} -> Account;
    {error, _} ->
      database:put_account(Account),
      Account
  end.

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when there is not enough money in the sender account.

-spec transfer(account_number(), account_number(), money()) ->
  {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
  | {ok, unique_id()}.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->

  TransferFunction =
    fun() ->
      MaybeAccountSender = database:get_account(SenderAccountNumber),
      MaybeAccountReceiver = database:get_account(ReceiverAccountNumber),
      %todo: Sender muss ungleich Empfänger sein
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
              NewAccountSender = AccountSender#account{amount = (AccountSenderAmount - Amount)},
              NewAccountReceiver = AccountReceiver#account{amount = (AccountReceiverAmount + Amount)},
              database:put_transfer(Transfer),
              database:put_account(NewAccountSender),
              database:put_account(NewAccountReceiver),
              events:put_event(#transaction_succeeded{transaction_id = Transfer#transfer.id, timestamp = Transfer#transfer.timestamp, from_account_number = Transfer#transfer.from_account_number, to_account_number = Transfer#transfer.to_account_number, amount = Transfer#transfer.amount}),
              {ok, TransferId};
            true ->
              {error, insufficient_funds}
          end
      end
    end,

  database:atomically(TransferFunction).

