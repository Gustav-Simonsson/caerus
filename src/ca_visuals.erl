%%%----------------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%%     Misc chart / pretty print functions
%%% @end
%%% Created : 04 Oct 2013 by Gustav Simonsson <gustav.simonsson@gmail.com>
%%%----------------------------------------------------------------------------
-module(ca_visuals).

-compile(export_all).

-include("ca.hrl").

%%%============================================================================
%%% API
%%%============================================================================
pretty_print_single_market_data(MarketData) ->
    [Label, LastTradeprice, LastTradeTime,
     PrimaryName, _PrimaryCode, _SecondaryCode
     % SellOrders, BuyOrders,
     %_RecentTrades
    ] =

        [?GV(Key, MarketData) ||
            Key <- [?LABEL,
                    ?LASTTRADEPRICE,
                    ?LASTTRADETIME,
                    ?PRIMARYNAME,
                    ?PRIMARYCODE,
                    ?SECONDARYCODE
                    %?RECENTTRADES
                    %?SELLORDERS,
                    %?BUYORDERS
                   ]],
    SecondsAgo = secs_since_last_trade(LastTradeTime),

    TemplateLines =
        [
         "=====================================================",
         "~s ~s ~p BTC ~p seconds ago ",
         "",
         "====================================================="
        ],
    Params = [PrimaryName,
              Label,
              list_to_float(binary:bin_to_list(LastTradeprice)),
              SecondsAgo],
    Template = lists:append([Line ++ "~n" || Line <- TemplateLines]),
    io:format(Template, Params),
    ok.

pretty_print_my_trades(Trades0) ->
    Sort = fun(Tag) -> fun(PL1, PL2) -> ?GV(Tag, PL1) > ?GV(Tag, PL2) end end,
    SortBy = fun(Tag, L) -> lists:sort(Sort(Tag), L) end,
    Filter = fun(Tag, Value) -> fun(PL) -> ?GV(Tag, PL) == Value end end,
    Pick = fun(A,B,Op) -> case erlang:Op(A, B) of true -> A; _ -> B end end,

    [FirstTrade | Trades] = lists:map(fun({L}) -> L end, Trades0),
    FirstMarketId = ?GV(?MARKETID, FirstTrade),
    FirstMarketIdTrades = lists:filter(Filter(?MARKETID, FirstMarketId), Trades),
    %
    Traverse =
        fun(PL, {First, Last, Cheapest, Priciest}) ->
                Secs = calendar:datetime_to_gregorian_seconds(
                         ca_util:cryptsy_datetime_to_calendar_datetime(
                           ?GV(?DATETIME, PL))),
                TradePrice = erlang:binary_to_float(?GV(?TRADEPRICE, PL)),
                NewFirst    = Pick(First,    Secs,       '<'),
                NewLast     = Pick(Last,     Secs,       '>'),
                NewCheapest = Pick(Cheapest, TradePrice, '<'),
                NewPriciest = Pick(Priciest, TradePrice, '>'),
                NewPL = [{?SECS, Secs}] ++ PL,
                {NewPL, {NewFirst, NewLast, NewCheapest, NewPriciest}}
        end,
    %?info("HURR ~p", [FirstMarketIdTrades]),

    {FirstTrades, {First, Last, Cheapest, Priciest}} =
        lists:mapfoldl(Traverse, {now_secs(), 0, 1010011100110100111001, 0},
                    FirstMarketIdTrades),
    ?info("~p", [{First, Last, Cheapest, Priciest}]),
    H = 40,
    W = 80,
    Wi = (Last - First) / W,
    Hi = (Priciest - Cheapest) / H,
    AddCoords =
        fun(PL) ->
                TradePrice = erlang:binary_to_float(?GV(?TRADEPRICE, PL)),
                Secs = ?GV(?SECS, PL),
                [{?Y, round((Priciest - TradePrice) / Hi)},
                 {?X, round((Last - Secs) / Wi)}] ++ PL
        end,
    T2 = lists:map(AddCoords, FirstTrades),
    SortByYThenX =
        fun(PL1, PL2) -> case (Y1 = ?GV(?Y, PL1)) == (Y2 = ?GV(?Y, PL2)) of
                             true -> (Sort(?X))(PL1, PL2);
                             false -> Y1 > Y2
                         end end,
    T3 = lists:sort(SortByYThenX, T2),
    ?info("~p", []),
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
secs_since_last_trade(LastTradeTime) ->
    now_secs() - (3600 * 5) -
        calendar:datetime_to_gregorian_seconds(
          ca_util:cryptsy_datetime_to_calendar_datetime(LastTradeTime)).

now_secs() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
