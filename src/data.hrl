-type money() :: number().
-type unique_id() :: integer().
-type account_number() :: integer().

-record(event, 
  {index :: non_neg_integer(),
   type :: atom(),
   content :: term()}).

-record(account, 
  {account_number :: account_number(),
   amount :: money()}).
-record(transfer,
  {id :: unique_id() | nil,
   timestamp :: erlang:timestamp(), 
   from_acc_nr :: account_number(), 
   to_acc_nr :: account_number(),
   amount :: money()}).
