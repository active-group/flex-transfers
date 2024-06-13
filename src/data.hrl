-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account,
{account_number :: account_number(),
  amount :: money()}).
-record(transfer,
{id :: unique_id(),
  timestamp :: erlang:timestamp(),
  from_account_number :: account_number(),
  to_account_number :: account_number(),
  amount :: money()}).


%% API Stuff
-record(ok, {identifier :: number(), sender :: binary()}).
-record(account_created,
{account_number :: number(),
  given_name :: binary(),
  surname :: binary(),
  amount :: number(),
  person_id :: number()}).

-record(transaction_succeeded,
{transaction_id :: unique_id(),
  from_account_number :: account_number(),
  to_account_number :: account_number(),
  amount :: money(),
  timestamp :: erlang:timestamp()}).
