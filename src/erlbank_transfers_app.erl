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
                                             {"/transfers/create", web_frontend, create_transfer}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),
    AccountNode = node_util:node_from_env(accounts, "ACCOUNTS_HOST"),
    {ok, Pid} = transfers_server:start(0, AccountNode),
    register(transfers_server, Pid),
    account_connector:init(local),
    start_cowboy(),
    erlbank_transfers_sup:start_link().

stop(_State) ->
    ok.

