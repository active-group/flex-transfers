%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/transfers/create", web_frontend, create_transfer}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).


start_sender() ->
  transfer_succeeded_sender:start().


start(_StartType, _StartArgs) ->
    database:init_database(),
    events:init_events(),
    start_sender(),
    start_cowboy(),
    erlbank_monolithic_sup:start_link().

stop(_State) ->
    ok.

