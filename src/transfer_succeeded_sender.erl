-module(transfer_succeeded_sender).

%% API
-export([send_loop/0, start/0]).
-include("data.hrl").
-include("events.hrl").

send_loop() ->
  case events:get_all_events() of
    [] -> send_loop();
    [First | _Rest] ->
      io:format("Sending message...~n"),
      Response = gen_server:call({statements, node_util:node_from_env(statements, "")}, First#event.payload),
      case Response of
        #ok{identifier = TransactionId} -> events:delete_transaction_succeeded_event(TransactionId)
      end,
      send_loop()
  end.

start() ->
  Pid = spawn(?MODULE, send_loop, []),
  {ok, Pid}.
