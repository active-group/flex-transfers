-module(commsService).
-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-behaviour(gen_server).

-spec init(non_neg_integer()) -> {ok, non_neg_integer()}.
init(LastEventId) -> {ok, LastEventId}.

handle_cast(Message, State) ->ok.

handle_call(get, _From, State) -> ok.

