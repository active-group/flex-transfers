-module(business_logic).
-include("data.hrl").
-export([transfer/3]).


-spec ts() -> erlang:timestamp().
ts() -> os:timestamp().


-spec transfer(account_number(), account_number(), money()) ->
                                            {ok, unique_id()} | 
                                            {error, sender_account_not_found | 
                                                    receiver_account_not_found |
                                                    insufficient_funds}.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->

    Tx = #transfer{id = nil,
                      timestamp = ts(),
                      from_acc_nr = SenderAccountNumber,
                      to_acc_nr = ReceiverAccountNumber,
                      amount = Amount},

    transfer_handler:handle_transfer(Tx).
