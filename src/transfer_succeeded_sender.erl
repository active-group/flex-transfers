-module(transfer_succeeded_sender).

%% API
-export([send_loop/0, start/0, init/1, handle_call/3, handle_cast/2, ack_handle_start/0]).
-behaviour(gen_server).
-include("data.hrl").
-include("events.hrl").

send_loop() ->
  case events:get_all_events() of
    [] -> send_loop();
    [First | _Rest] ->
      io:format("Sending message...~n"),
      gen_server:cast({statements, node_util:node_from_env(statements, "")}, First#event.payload, 5000),
      io:format("Received ~w~n", [Response]),
      case Response of
        #ok{identifier = TransactionId} -> events:delete_transaction_succeeded_event(TransactionId);
        {error,sender_account_not_found} -> none;
        true -> none
      end,
      send_loop()
  end.

start() ->
  Pid = spawn(?MODULE, send_loop, []),
  {ok, Pid}.


init([]) -> {ok, []}.
handle_call(#ok{ identifier = _TransactionId}, _From, _) -> noop.
handle_cast(Ack, _) ->
  case Ack of
    #ok{identifier = TransactionId} -> events:delete_transaction_succeeded_event(TransactionId);
    {error,sender_account_not_found} -> none;
    true -> none
    end,
  {noreply, []}.

ack_handle_start() ->
  gen_server:start(?MODULE, [], [{debug, [trace]}]).
