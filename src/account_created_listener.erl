-module(account_created_listener).
-export([listen/0, start/0, mockRecipient/0, startMockRecipient/0]).
-include("data.hrl").

listen() ->
  receive
    #account_created{account_number = AccountNumber, amount = Amount} ->
      Account = business_logic:make_account(AccountNumber, Amount),
      case Account of
        #account{account_number = AccountNumber} ->
          gen_server:cast({accounts, node_util:node_from_env(accounts, "")}, #ok{identifier = AccountNumber, sender = <<transfers>>})
      end,
      listen()
  end.

start() ->
  Pid = spawn(?MODULE, listen, []),
  {ok, Pid}.

mockRecipient() ->
  receive
    M -> io:format("Received ok with identifier ~w~n", [M])
  end.

startMockRecipient() ->
  Pid = spawn(?MODULE, mockRecipient, []),
  {ok, Pid}.