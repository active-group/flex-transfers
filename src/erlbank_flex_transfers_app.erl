
%%%-------------------------------------------------------------------
%% @doc erlbank_flex_transfers public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_flex_transfers_app).

-behaviour(application).

-export([start/2, stop/1]).



check_is_set(Var) ->
    case os:getenv(Var) of
        false ->
            % FIXME: log it with logger?
            io:format("Missing var ~s~n", [Var]),
            halt(1);
        _ -> ok
    end.


start_cowboy() ->

    %% Cowboy test code
    Dispatch = cowboy_router:compile([%% _ = hostmatch
                                      %% _, [..] = list of paths
                                      %% [] initial state
                                      {'_', [{"/", web_frontend, index},
                                             {"/transfers/create", web_frontend, add}]}
                                     ]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).

start(_StartType, _StartArgs) ->

    logger:info("Starting transfers-service: ~p~n", [node()]),

    start_cowboy(),

    database:init_database(),

    check_is_set("ACCOUNTS_HOST"),
    AccountNode = list_to_atom("accounts@" ++ os:getenv("ACCOUNTS_HOST")),
    Res = erlbank_flex_transfers_sup:start_link(AccountNode),

    logger:info("Consuming accounts on: ~p~n", [AccountNode]),
    Res.

stop(_State) ->
    database:close_tables(),
    database:destroy_tables().

%% internal functions
