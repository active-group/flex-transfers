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
    given_name :: binary(),
    surname :: binary(),
    amount :: number(),
    person_id :: number()}).

Output
RegistryName: accounts
-record(ok, {sender: binary(), account_number: number()})


## Out
### transaction_succeeded

Output
RegistryName: statements
-record(transfer,
        {id :: unique_id(),
        timestamp :: erlang:timestamp(),
        from_account_number :: account_number(),
        to_account_number :: account_number(),
        amount :: money()}).

Input
RegistryName: transfer_succeeded_ack
-record(ok, {sender: binary(), transaction_id: number()})