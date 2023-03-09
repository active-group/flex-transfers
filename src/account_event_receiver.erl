-module(account_event_receiver).
-include("data.hrl").

%% API
-export([receive_accounts/0, filter_for_new_account_events/1, map_to_account/1]).

receive_accounts() ->
  From = database:last_event_id(),
  Events = gen_server:call(account_feed, {events, From}),
  case Events of
    [{_, Index, _, _} | _Rest] ->
      Filtered = filter_for_new_account_events(Events),
      database:put_accounts(Filtered),
      database:put_event_id(Index)
  end,
  ok.

-spec filter_for_new_account_events([{event, number(), new_person_event | new_account_event, any}]) -> [#account{}].
filter_for_new_account_events(Events) ->
  lists:filtermap(
    fun(Event) ->
      case Event of
        {_, _, new_account_event, Content} -> {true, map_to_account(Content)};
        _ -> false end
    end,
    Events).

-spec map_to_account({number(), number(), number()}) -> #account{}.
map_to_account({AccountNumber, _PersonId, InitialBalance}) ->
  #account{account_number = AccountNumber, amount = InitialBalance}.
