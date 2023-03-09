-module(transfer_feed).

%% API
-export([
  init/1, start/0, stop/0,
  handle_cast/2,
  handle_call/3
]
).

-include("data.hrl").

% Interface
-behavior(gen_server).

-spec start() -> {ok, pid()}.
start() ->
  gen_server:start({local, transfer_feed}, transfer_feed, [], [{debug, [trace]}]).

stop() ->
  gen_server:stop(transfer_feed).

%Callback
init(_) ->
  {ok, started}.

handle_cast(_Request, State) ->
  {noreply,
    State}. % new state


handle_call({events, From}, _From, State) ->
  {reply,
    get_events_from(From), % reply
    State}. % new state

-spec get_events_from(number()) -> {ok, [{event, unique_id(), transfer_event,
  {unique_id(), erlang:timestamp(), account_number(), account_number(), money()}
}]} | {error, any()}.
get_events_from(Id) ->
  TransferList = database:get_all_transfers(),
  lists:filtermap(
   fun(Transfer) ->
     #transfer{id = TransferId, timestamp = Timestamp, from_account_number = FromAccountNumber, to_account_number = ToAccountNumber, amount = Amount} = Transfer,
       case TransferId >= Id of
         true -> {true, {event, TransferId, transfer_event, {TransferId, Timestamp, FromAccountNumber, ToAccountNumber, Amount}}};
         false -> false
       end
   end,
    TransferList).
