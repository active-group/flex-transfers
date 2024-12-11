# Erlbank Transfers

Erlbank Legacy System


## Interface Definition
```
Es existiert ein transaction_server (gen_server) mit den Funktionen get_transactions und get_transactions_from.


-record(get_transactions,{accountId :: number()}).
-> List(-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}))
-record(get_transactions_from,{accountId::number(),start_transaction::number()}).
-> List(-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}))

```

## Build

```
$ rebar3 compile
```

## Check

You can run the dialyzer via:

```
$ rebar3 dialyzer
```

## Test

You can run the tests in the `tests/` directory via:

```
$ rebar3 eunit
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

You can set a short name via:

```
$ rebar3 shell --sname=transfers
```

The web-frontend is served at http://localhost:8000/
