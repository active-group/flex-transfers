%%% @author Author_Name <Author_Email>
%%% @copyright (C) 2024, Author_Name
%%% @doc 
%%%
%%% @end
%%% Created : 11 Dec 2024 by Author_Name <Author_Email>
-module(account_connector).

-include("data.hrl").

-export([get_account/1,put_account/1,init/1]).


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
%lokaler modus
get_account(AccountNumber) ->
    database:get_account(AccountNumber).


-spec put_account(#account{}) -> ok.
%lokal
put_account(Account) ->
    database:put_account(Account).



-spec init(pid() | local) -> ok | not_available.
%init(PID) -> todo;
init(local) ->
    Account1 = #account{account_number = 1,
                   person_id = 42,
                   amount = 1000},
    Account2 = #account{account_number = 2,
                   person_id = 55,
                   amount = 500},
    Account3 = #account{account_number = 3,
                   person_id = 69,
                   amount = 100},
    database:put_account(Account1),
    database:put_account(Account2),
    database:put_account(Account3),
    ok.
% initialisiere mit einer PID
% -> an PID gibts einen prozess ^= produktivsystem und fragen echte daten ab
% -> gibts nicht -> error 
% Local -> use mockup

% Entscheidung/Flag was wir tun müssen

%Case: Produktivsystem
% -> Anfrage an Schnittstelle von Accounts Microservice
% erwartet 
% {error, not_found}  -ODER
%  {ok, #Account}

% -> database:put_account(NewAccountSender),

% Case: Entwickungsumgebung
% -> Anfrage an Mockup
% erwartet 
% {error, not_found}  -ODER
%  {ok, #Account}
