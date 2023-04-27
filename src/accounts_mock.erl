-module(accounts_mock).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/1, start_demo_link/0]).

start_demo_link() ->
    start_link([{person, 1, <<"Mike">>, <<"Sperber">>},
                {account, 15, 1, 1500},
                {person, 2, <<"Simon">>, <<"Härer>">>},
                {account, 16, 2, 1600}]).

start_link(Msgs) ->
    gen_server:start_link({local, account_service}, ?MODULE, Msgs, []).

init(Msgs) -> {ok, Msgs}.

handle_cast(_Msg, Msgs) -> {noreply, Msgs}.

handle_call(Pid, _From, Msgs) ->
    lists:foreach(fun (Msg) -> gen_server:cast(Pid, Msg) end, Msgs),
    {reply, ok, []}.
                          
                          

