-module(account_created_listener).

-include("data.hrl").
-behaviour(gen_server).


-export([account_listener_start/0, handle_cast/2, handle_call/3, init/1]).

init([]) -> {ok, []}.
handle_call(Message, _From, _) -> noop.
-spec handle_cast(#account_created{}, none()) -> none().
handle_cast(#account_created{account_number = AccountNumber, amount = Amount}, _) ->
  Account = business_logic:make_account(AccountNumber, Amount),
  case Account of
    #account{account_number = AccountNumber} ->
      gen_server:cast({accounts, node_util:node_from_env(accounts, "ACCOUNTS_HOST")}, #ok{sender = transfers, identifier = AccountNumber})
  end,
  {noreply, []}.

account_listener_start() ->
  gen_server:start(?MODULE, [], [{debug, [trace]}]).