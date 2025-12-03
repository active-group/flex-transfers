%%%-------------------------------------------------------------------
%%% @author nicola
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2025 1:37 PM
%%%-------------------------------------------------------------------
-module(event_sink).
-include("event.hrl").
%% API
-export([start/1, handle_call/3, init/1, handle_cast/2]).
-behavior(gen_server).

-type state() :: state.

call(Pid, Request) ->
  case gen_server:call(Pid, Request) of
    ok -> ok;
    _ -> call(Pid, Request)
  end.

-spec start(pid()) -> any().
start(SourcePid) ->
  gen_server:start(?MODULE, SourcePid, [{debug, [trace]}]).

-spec init(pid()) -> {ok, state()}.
init(SourcePid) ->
  call(SourcePid , #register{}),
  {ok, state}.

-spec handle_call(any(), pid(), state()) -> {reply, any(), state()}.
handle_call(#account_creation_event{account_number = AccountNumber}, _From, State) ->
  business_logic:make_account(AccountNumber),
  {reply, ok, State};
handle_call(_Any, _From, State) ->
  {reply, ok, State}.


-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Any, State) ->
  {noreply, State}.