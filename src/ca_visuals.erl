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
pretty_print_my_trades(MyTrades0, MarketData0, Info) ->
    Sort = fun(Tag) -> fun(PL1, PL2) -> ?GV(Tag, PL1) > ?GV(Tag, PL2) end end,
    SortBy = fun(Tag, L) -> lists:sort(Sort(Tag), L) end,
    Filter = fun(Tag, Value) -> fun(PL) -> ?GV(Tag, PL) == Value end end,
    Pick = fun(A,B,Op) -> case erlang:Op(A, B) of true -> A; _ -> B end end,
    LabelMatch = fun(Cur) -> Filter(?LABEL, <<Cur/binary, "/BTC">>) end,

    Markets = get_markets(),

    {[{<<"balances_available">>,{Balances0}} | _]} = Info,
    Balances = lists:filter(fun({_, FB}) -> 0 /= binary_to_float(FB) end,
                            Balances0),

    {[{<<"markets">>, {MarketData1}}]} = MarketData0,
    MarketData = lists:map(fun({_Label, {L}}) -> L end, MarketData1),

    Trades = SortBy(?DATETIME, lists:map(fun({L}) -> L end, MyTrades0)),

    BTCValue =
        fun({<<"BTC">>, FB}) ->
                binary_to_float(FB);
           ({Cur, FB}) ->
                [PL] = lists:filter(LabelMatch(Cur), MarketData),
                LTP = ?GV(?LASTTRADEPRICE, PL),
                binary_to_float(FB) *  binary_to_float(LTP)
        end,
    TotalBTC = lists:sum(lists:map(BTCValue, Balances)),

    AltsAcc = lists:map(fun({Cur, _}) ->
                                [PL] = lists:filter(LabelMatch(Cur), Markets),
                                {?GV(?MARKETID, PL), Cur, []}
                        end, lists:keydelete(<<"BTC">>, 1, Balances)),

    SumUp =
        fun(PL, Acc) ->
                G = fun(Tag) -> ?GV(Tag, PL) end,
                TP  = binary_to_float(G(?TRADEPRICE)),
                Q   = binary_to_float(G(?QUANTITY)),
                MId = G(?MARKETID),
                TT  = G(?TRADETYPE),
                NegateBuys =
                    fun(Buy, 0) -> {Buy, 0};
                       ({BTP, BQ}, SellQLeft) when BQ < SellQLeft ->
                            {{BTP, 0}, SellQLeft - BQ};
                       ({BTP, BQ}, SellQLeft) ->
                            {{BTP, BQ - SellQLeft}, 0}
                    end,

                NewAcc =
                    case lists:keyfind(MId, 1, Acc) of
                        false -> Acc;
                        {M, C, Buys} ->

                            case TT of
                                ?TRADETYPE_BUY ->
                                    case M of <<"70">> ->
                                    io:format("~p + ", [Q]);
                                        _ -> ok
                                    end,

                                    New = {M, C, [{TP, Q} | Buys]},
                                    lists:keyreplace(MId, 1, Acc, New);
                                ?TRADETYPE_SELL ->

                                    {NewBuys, _} =
                                        lists:mapfoldl(NegateBuys, Q, Buys),
                                    NewAltAcc = {M, C, NewBuys},
                                    lists:keyreplace(MId, 1, Acc, NewAltAcc)
                            end
                    end
        end,
    X = lists:foldl(SumUp, AltsAcc, Trades),
    %?info("~p", [float_round(TotalBTC, 4)]),

    ?info("~p", [lists:last(lists:reverse(X))]),
    ?info("~p", [lists:last(lists:reverse(AltsAcc))]),
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

float_round(FloatBin, Precision) when is_binary(FloatBin) ->
    float_round(binary_to_float(FloatBin), Precision);
float_round(FloatStr, Precision) when is_list(FloatStr) ->
    float_round(list_to_float(FloatStr), Precision);
float_round(Float, Precision) ->
    io_lib:format("~." ++ integer_to_list(Precision) ++ "f", [Float]).
