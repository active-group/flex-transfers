%%%-------------------------------------------------------------------
%% @doc erlbank_transfers public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_transfers_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/transfers/create", web_frontend, create_transfer}
    ]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),

    AccountNode = node_util:node_from_env(accounts, "ACCOUNTS_HOST"),
    logger:info("Contacting accounts on: ~p~n", [AccountNode]),
    event_sink:start({event_sender, AccountNode}),
    {ok, Pid} = event_server:start(),
    register(event_sender, Pid),
    erlbank_transfers_sup:start_link().

stop(_State) ->
    ok.

