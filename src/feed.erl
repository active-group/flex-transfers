-module(feed).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/0, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, transfer_feed}, ?MODULE, [], []).

init([]) -> {ok, []}.

-spec events_from(non_neg_integer()) -> list(#event{}).
events_from(N) ->
    database:transfer_events_from(N).

handle_cast(Msg, State) ->
    logger:error("Received illegal message: ~p~n", [Msg]),
    {noreply, State}.

handle_call({events, From}, _, State) ->
    {reply, events_from(From), State} ;

handle_call(Msg, _, State) ->
    logger:error("Received illegal message: ~p~n", [Msg]),
    {reply, undefined, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
