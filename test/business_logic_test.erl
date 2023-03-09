-module(business_logic_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/data.hrl").

setup() ->
  database:init_database().

cleanup(_) -> ok.

main_test_() ->
  {inorder,
    {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun get_account/1]
    }}.


get_account(_) ->
  fun() ->
    Account = #account{account_number = 1, amount =  1000},
    database:put_account(Account),
    ?assertEqual(business_logic:get_account(1), {ok, Account})
  end.
