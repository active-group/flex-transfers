-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account, {account_number :: account_number(), amount :: money()}).
-record(transfer,
        {id :: unique_id(),
         timestamp :: erlang:timestamp(),
         from_account_number :: account_number(),
         to_account_number :: account_number(),
         amount :: money()}).
-record(transfer_event,
        {source :: transfer_service,
         eventId :: non_neg_integer(),
         accountIdSender :: non_neg_integer(),
         accountIdReceiver :: non_neg_integer(),
         amount :: number(),
         timestamp :: erlang:timestamp(),
         sent :: boolean()}).
-record(get_transfer_events_since, {since :: non_neg_integer(), receiver_pid :: pid()}).
