
-module(dev).
-include("data.hrl").
-export([setup/0]).


setup() ->
    database:close_tables(),
    database:destroy_tables(),
    database:init_database(),
    database:put_event(
      #event{index = 0,
             type = new_account_event,
             content = {0, 1000}}),
    database:put_event(
      #event{index = 1,
             type = new_account_event,
             content = {1, 1000}}).
