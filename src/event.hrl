-include("data.hrl").

-record(account_creation_event,
    {account_number :: account_number(), person_id :: unique_id()}).
-record(transfer_creation_event,
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}).
-record(person_creation_event,
     {id :: unique_id(), given_name :: binary(), surname :: binary()}).
-record(register,
     {}).