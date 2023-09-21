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
         accountIdSender :: non_neg_integer(),
         accountIdReceiver :: non_neg_integer(),
         amount :: number(),
         timestamp :: erlang:timestamp()}).
-record(get_transfer_events_since, {since :: non_neg_integer(), receiver_pid :: pid()}).

-record(account_event,
  {id :: integer(),
  eventType:: account_created,
  account_number:: integer(),
  givenName:: string(),
  surname:: string()}).

  -record(get_account_events_since, {
        since :: integer(), receiver_pid :: pid()
  }).