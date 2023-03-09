# Flex Erlbank Transfer Service

Service to transfer money between account within the Erlbank.

## Events interface

On the Erlang node this service runs on, there is a `gen_server`
process in the local registry called `transfer_feed`. This serves
events in the following format:

```
{event, Index, Type, Content}
```

- `Index` is a non-negative integer, by which events are ordered in time
- `Type` is actually only `transfer_event`
- `Content` depends on `Type`

For `Type` `transfer_event` the `Content` is a tuple

```
{TransferId, Timestamp, FromAccountNumber, ToAccountNumber, Amount}
```

Where:

- `TransferId` is a unique integer
- `Timestamp` is an `erlang:timestamp`
- `FromAccountNumber` and `ToAccountNumber` are unique integers generated by the account service
- `Amount` is a number denoting the transferred amount in Euro.

These events are served from the `transfer_feed` process via a call like so:

```
gen_server:call(transfer_feed, {events, From})
```

This returns a list of all available event tuples starting with the `From` index, in ascending order.
(Or the least index higher than `From` - specifying 0 here fetches *all* events.)

## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/accounts

## Run locally using docker

This project comes with a docker container. It is built using

```
docker build . -t accounts
```

in the root directory of the project. To run the docker container call

 ```
 docker run -p 8000:8000 -e "RELX_REPLACE_OS_VARS=true" -e "NODE_NAME=any_name" accounts
 ```

Running with docker we are able to configure the node name of the erlang node
using the `NODE_NAME` env var. To do so, relx must be informed that the
vm.args file contains env vars via `RELX_REPLACE_OS_VARS`.

If the docker container is up and running, the web-frontend can be found at
http://localhost:8000/accounts

## Testing

rebar3 & eunit are used for testing. To test the service use

```
rebar3 eunit
```

To test it within the docker container use

```
docker run accounts test
```

## Release

A release can be built using

```
rebar3 release
```

_____________________

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
