# Transfer Service

Service to create transfers between accounts


## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The application consumes an account feed. To know, where this feed it, the node
identifier has to be passed to the service, using an env variable. The
application can be run in an development environment including a REPL using the
following command:

```
$ ACCOUNT_NODE=node_name rebar3 shell
```

When the node `node_name` is not reachable, there will be an error, but the service will
continue running. To get data into the database anyways, call `dev:setup().`.

The web-frontend is served on http://localhost:8000/transfers


## Run locally using docker

This project comes with a docker container. It is built using 

```
docker build . -t transfers
```

in the root directory of the project. To run the docker container call
 
 ```
 docker run -p 8000:8000 -e "RELX_REPLACE_OS_VARS=true" -e "NODE_NAME=any_name" -e "ACCOUNT_NODE=some_name" transfers
 ```
 
 Running with docker we are able to configure the node name of the erlang node
 using the `NODE_NAME` env var. To do so, relx must be informed that the 
 vm.args file contains env vars via `RELX_REPLACE_OS_VARS`.
 
 If the docker container is up and running, the web-frontend can be found at
 http://localhost:8000/transfers


## Testing

rebar3 & eunit are used for testing. To test the service use

```
rebar3 eunit
```

To test it within the docker container use

```
docker run transfers test
```


## Release

A release can be built using 

```
rebar3 release
```

