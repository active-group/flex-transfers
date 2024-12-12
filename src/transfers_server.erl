-module(transfers_server).

-behavior(gen_server).
-export([init/1,start/2,add/2,get_transfers/2,get_all_transfers_from/2,handle_cast/2,handle_call/3,handle_info/2]).
-include("data.hrl").

start(InitialN, NodeName) ->
    gen_server:start(transfers_server, {InitialN, NodeName}, [{debug, [trace]}]).

init({InitialN, NodeName}) ->
    % in dem neuen Prozeß, self()
    timer:send_interval(5000, refresh),
    {ok, {InitialN, NodeName}}.

% cast: asynchrone Nachricht an den Server
-record(add, {inc :: number()}).

add(Pid, Inc) ->
    gen_server:cast(Pid, #add{inc = Inc}).

handle_cast(#add{inc = Inc}, N) ->
    {noreply, N + Inc}.

handle_info(refresh, N) ->
    account_connector_query(N),
    {noreply, N};
handle_info(_,State) ->
    {noreply,State}.

% call: RPC mit Antwort
% -record(query, {transferid :: number()}).
-record(get_transfers, {accountNumber :: number()}).
% -record(get_transfers_from, {accountNumber :: number(), transferId :: number()}).
-record(get_all_transfers_from, {transferId :: number()}).

% get_transfers_from(Pid, AccountNumber, TransferId) ->
%     gen_server:call(Pid, #get_transfers_from{accountNumber = AccountNumber, transferId = TransferId}).

% get_all_transfers_from(Pid, TransferId) ->
%     gen_server:call(Pid, #get_all_transfers_from{transferId = TransferId}).
get_all_transfers_from(ServerName, TransferId) ->
    gen_server:call(ServerName, #get_all_transfers_from{transferId = TransferId}).

get_transfers(Pid, AccountNumber) ->
    gen_server:call(Pid, #get_transfers{accountNumber = AccountNumber}).

% handle_call(#get_transfers_from{accountNumber = AccountNumber, transferId = TransferId}, _From, _N) ->
%     {reply,
%      database:get_transfers_from(AccountNumber, TransferId),   % Antwort
%      TransferId }; % neuer Zustand
handle_call(#get_all_transfers_from{transferId = TransferId}, _From, State) ->
    {reply,
     database:get_transfers_from(TransferId),   % Antwort
     State }; % neuer Zustand
handle_call(#get_transfers{accountNumber = AccountNumber}, _From, State) ->
    {reply,
     database:get_all_transfers(AccountNumber),   % Antwort
     State }. % neuer Zustand    

-spec add_accounts(list(#account{}), number()) -> number().
add_accounts([],Newest) ->
    {ok,Newest};    
add_accounts(Accounts,_Newest) ->
    [{account_dto,Acc_Num,PersonId,_,_,Amount} | Rest] = Accounts,
    database:put_account(#account{account_number = Acc_Num,person_id=PersonId,amount=Amount} ),
    add_accounts(Rest,Acc_Num).

-spec account_connector_query({number(),nodename}) -> {ok,number()}.
account_connector_query({InitialN, NodeName}) ->
    io:format("get accounts from ~w~n", [InitialN]),
    try gen_server:call({account_server, NodeName}, {subscribe,InitialN,self()}) of
        Accounts ->
            io:format("answer ~w~n",[Accounts]), 
            add_accounts(Accounts,InitialN),
            ok
    catch
        _:_ -> 
            io:format("get accounts failed~n"),
            noresponse
    end.
