
%%%-------------------------------------------------------------------
%% @doc erlbank_flex_transfers public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_flex_transfers_app).

-behaviour(application).

-export([start/2, stop/1]).

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

getenv(Var, Cont, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Cont(Value)
    end.

start(_StartType, _StartArgs) ->

    logger:info("Starting transfers-service: ~p~n", [node()]),

    start_cowboy(),

    database:init_database(),

    % accounts_mock:start_demo_link(),
    AccountNode = getenv("ACCOUNTS_HOST",
                         fun (AccountsHost) -> list_to_atom("accounts@" ++ AccountsHost) end,
                         node()),
    Res = erlbank_flex_transfers_sup:start_link(AccountNode),

    logger:info("Consuming accounts on: ~p~n", [AccountNode]),
    Res.

stop(_State) ->
    database:close_tables(),
    database:destroy_tables().

%% internal functions
