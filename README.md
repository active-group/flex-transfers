# Erlbank Transfers

Erlbank Legacy System


## Interface Definition
```
Es existiert ein transfers_server (gen_server) mit den Funktionen get_transfers und get_all_transfers_from.

gen_server Call get_transfers:
-record(get_transfers,{accountNumber :: number()}).
-> List(-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}))

Beispiel: alle Transfers für Account-Nummer 1
transfers_server:get_transfers(Pid, 1).

gen_server Call get_all_transfers_from:
-record(get_all_transfers_from,{start_transfer::number()}).
-> List(-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}))

Beispiel: alle Transfers ab Transfer-Id 3
transfers_server:get_all_transfers_from(Pid, 3).
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
