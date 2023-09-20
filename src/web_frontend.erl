
-module(web_frontend).
-include("data.hrl").
-export([init/2]).


-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

-spec transfer_error() -> binary().
transfer_error() ->
    << "
      <p> An error occured: ~p </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

-spec transfer_success() -> binary().
transfer_success() ->
            << "
      <p> Transfer with id ~p successfully created </p> ~n
               <a href=\"/\"> Back </a>
    " >>.

-spec transfer_form() -> binary().
transfer_form() ->
                    << "
<h3> Create transfer </h3>
                       <form method=\"post\" action=\"/transfers/create\">
  <label for=\"transfers_from\"> From (account number) </label>
  <input type=\"text\" id=\"transfers_from\" name=\"transfers_from\" />

  <label for=\"transfers_to\"> To (account number) </label>
  <input type=\"text\" id=\"transfers_to\" name=\"transfers_to\" />

  <label for=\"transfers_amount\"> Amount </label>
  <input type=\"text\" id=\"transfers_amount\" name=\"transfers_amount\" />

  <input type=\"submit\" value=\"Create transfer\" />
</form>" >>.


-spec transfer_template() -> string().
transfer_template() ->
    "<tr>
      <td> ~p </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
    </tr>".

-spec amount_to_string(money(), string(), number_formatter:locale()) -> string().
amount_to_string(Amount, Currency, Format) ->
    {ok, AmountExchanged} = exchange_service:exchange(Currency, Amount),
    AmountFormatted = number_formatter:format(Format, AmountExchanged),
    AmountFormatted ++ " " ++ Currency.

-spec transfer(#transfer{}, string(), number_formatter:locale()) -> string().
transfer(Transfer, Currency, Format) ->
    AccountNumber1 = Transfer#transfer.from_account_number,
    AccountNumber2 = Transfer#transfer.to_account_number,
    Amount = amount_to_string(Transfer#transfer.amount, Currency, Format),
    Date = date_formatter:format(Format, Transfer#transfer.timestamp),
    Id = Transfer#transfer.id,
    io_lib:format(transfer_template(), [Id, Date, Amount, AccountNumber1, AccountNumber2]).

index() ->
    io_lib:format("~s",
                  [transfer_form()]).


%% /transfers/create
init(Request, create_transfer) ->

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

    KeyValues = maps:from_list(KeyValuesL),
    SenderAccountNumber =  bin_to_int(maps:get(<<"transfers_from">>, KeyValues)),
    ReceiverAccountNumber = bin_to_int(maps:get(<<"transfers_to">>, KeyValues)),
    Amount = bin_to_int(maps:get(<<"transfers_amount">>, KeyValues)),

    Body = case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
               {ok, TransferId} ->
                   io_lib:format(transfer_success(), [TransferId]);
               {error, Error} ->
                   io_lib:format(transfer_error(), [Error])
           end,
    Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Request),
    {ok, Reply, []};


%% /index
init(Request, index) ->
    Reply = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Request),
    {ok, Reply, []}.
