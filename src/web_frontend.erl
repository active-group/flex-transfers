
-module(web_frontend).
-export([init/2]).

-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

error() ->
    << "
      <p> An error occured: ~p </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

success() ->
    << "
      <p> Transfer with id ~p successfully created. </p> ~n
       <a href=\"/\"> Back </a>
    " >>.


form() ->
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


init(Req, add) ->

    logger:info("Creating new transfer"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    SenderAccountNumber =  bin_to_int(maps:get(<<"transfers_from">>, KeyValues)),
    ReceiverAccountNumber = bin_to_int(maps:get(<<"transfers_to">>, KeyValues)),
    Amount = bin_to_int(maps:get(<<"transfers_amount">>, KeyValues)),

    Body =
        case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
            {ok, TxId} ->
                logger:info("Created new transfer with id ~p", [TxId]),
                io_lib:format(success(), [TxId]);
            {error, Err} ->
                logger:info("An error occured while creating transfer: ~p", [Err]),
                io_lib:format(error(), [Err])
        end,


    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
    {ok, Req2, []};


init(Req0, index) ->
    ResponseBody = form(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           ResponseBody,
                           Req0),
    {ok, Req, []}.
