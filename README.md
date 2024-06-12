# Erlbank Monolithic

Erlbank Legacy System

## Build

```
$ rebar3 compile
```


## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8001/


# API

## In
### account_created
Input
RegistryName: transfers
-record(account_created,
    {account_number :: number(),
    amount :: number()}).

Output
-record(ok, account_number: number())


## Out
### transaction_succeeded

Output
RegistryName: statements
-record(transfer,
    {transaction_id :: number(),
    from_account_number :: account_number(),
    to_account_number :: account_number(),
    amount :: money(),
    timestamp :: erlang:timestamp()}).

Input
-record(ok, transaction_id: number())