%%%-------------------------------------------------------------------
%% @doc erlbank_flex_transfers top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_flex_transfers_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(AccountNode) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [AccountNode]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([AccountNode]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => feed,
                   start => {feed, start_link, []}},
                  #{id => transfer_handler,
                    start => {transfer_handler, start_link, []}},
                  #{id => consumer,
                   start => {consumer, start_link, [AccountNode]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
