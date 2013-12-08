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
            Key <- [?MARKET_LABEL,
                    ?MARKET_LASTTRADEPRICE,
                    ?MARKET_LASTTRADETIME,
                    ?MARKET_PRIMARYNAME,
                    ?MARKET_PRIMARYCODE,
                    ?MARKET_SECONDARYCODE
                    %?MARKET_RECENTTRADES
                    %?MARKET_SELLORDERS,
                    %?MARKET_BUYORDERS
                   ]],
    SecondsAgo = secs_since_last_trade(binary:bin_to_list(LastTradeTime)),

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

pretty_print_my_trades(MyTrades) ->
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
secs_since_last_trade(LastTradeTime) ->
    [Y, M, D, H, Mins, S] =
        [list_to_integer(L) || L <- string:tokens(LastTradeTime, "- :")],
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) -
        (3600 * 5) -
        calendar:datetime_to_gregorian_seconds({{Y,M,D}, {H,Mins,S}}).
