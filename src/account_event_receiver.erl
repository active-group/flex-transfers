-module(account_event_receiver).

%% API
-export([receive_accounts/0, filter_for_new_account_events/1]).

receive_accounts() ->
  From = 0,
  % TODO implement From
  % From = database:get_max_from();
  gen_server:call(account_feed, {events, From}).

% {event, Index, Type, Content}
%{AccountNumber, PersonId, InitialBalance}
%new_account_event
%.

%-spec filter_for_new_account_events([Events]) -> [Account_content].
filter_for_new_account_events(Events) ->

  % {event, Index, Type, Content}
  %{AccountNumber, PersonId, InitialBalance}
  %Events = [{event, 1, new_person_event, {1, <<"Franz">>, <<"Huber">>}}, {event, 2, new_account_event, {1, 1, 1000}}],

  lists:filtermap(
    fun(Event) ->
      case Event of
        {_, _, new_account_event, Content} -> {true, Content};
        _ -> false end
    end,
    Events).

