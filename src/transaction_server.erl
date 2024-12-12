-module(transaction_server).

-behavior(gen_server).
-export([init/1,start/1,add/2,get_transactions/2,handle_cast/2,handle_call/3]).

start(InitialN) ->
    gen_server:start(transaction_server, InitialN, [{debug, [trace]}]).

init(InitialN) ->
    % in dem neuen Prozeß, self()
    {ok, InitialN}.

% cast: asynchrone Nachricht an den Server
-record(add, {inc :: number()}).

add(Pid, Inc) ->
    gen_server:cast(Pid, #add{inc = Inc}).

handle_cast(#add{inc = Inc}, N) ->
    {noreply, N + Inc}.

% call: RPC mit Antwort
% -record(query, {transferid :: number()}).
-record(get_transactions, {accountId :: number()}).

% get_transactions(Pid, TransferId) ->
%     gen_server:call(Pid, #query{transferid = TransferId}).
get_transactions(Pid, AccountId) ->
    gen_server:call(Pid, #get_transactions{accountId = AccountId}).

% handle_call(#query{}, _From, TransferId) ->
%     {reply,
%      database:get_transfers(TransferId),   % Antwort
%      TransferId }; % neuer Zustand
handle_call(#get_transactions{accountId = AccountId}, _From, _N) ->
    {reply,
     database:get_all_transfers(AccountId),   % Antwort
     AccountId }. % neuer Zustand    