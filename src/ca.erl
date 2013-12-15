%%%----------------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%%
%%% caerus server
%%%
%%% @end
%%% Created : 04 Oct 2013 by Gustav Simonsson <gustav.simonsson@gmail.com>
%%%----------------------------------------------------------------------------
-module(ca).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-include("ca.hrl").

-define(SERVER, ?MODULE).

-define(CURL_TIMEOUT, 180000).

-record(s, {markets = []
           }).

%%%============================================================================
%%% API
%%%============================================================================
view_my_trades() ->
    gen_server:call(?SERVER, view_my_trades, ?CURL_TIMEOUT + 100).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    Stop = fun() -> spawn(fun() -> timer:sleep(200),
                                   ca_app:stop() end) end,
    RequiredConfigs =
        [{C, application:get_env(?APP, C, missing)}
         || C <- [public_endpoint,
                  auth_endpoint,
                  public_api_key,
                  secret_api_key
                 ]],
    CheckConfig =
        fun({Config, missing}) ->
                ?error("Missing config parameter ~p, "
                       "please add it and start again", [Config]),
                Stop();
           ({_Config, _Val}) -> ok
        end,
    lists:foreach(CheckConfig, RequiredConfigs),
    case file:consult(?MARKETS_FILE) of
        {ok, [CryptsyMarkets]} ->
            {ok, #s{markets = CryptsyMarkets}};
        _ ->
            ?error("Missing cryptsy markets file ~p, "
                   "please add it using ca_util and start again",
                   [?MARKETS_FILE]),
            Stop(),
            {ok, #s{}}
    end.

handle_call(view_my_trades, _From, S) ->
    do_view_my_trades(),
    {reply, ok, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(Call, From, S) ->
    lager:error("~p: Unmatched call ~p from ~p~n", [?MODULE, Call, From]),
    {noreply, S}.

handle_cast(Cast, S) ->
    lager:error("~p: Unmatched cast ~p~n", [?MODULE, Cast]),
    {noreply, S}.

handle_info(Info, S) ->
    lager:error("~p: Unmatched info ~p~n", [?MODULE, Info]),
    {noreply, S}.

terminate(Reason, _S) ->
    lager:info("~p: terminating server: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
do_view_my_trades() ->
    Sort = fun(Tag) -> fun(PL1, PL2) -> ?GV(Tag, PL1) > ?GV(Tag, PL2) end end,
    SortBy = fun(Tag, L) -> lists:sort(Sort(Tag), L) end,
    Filter = fun(Tag, Value) -> fun(PL) -> ?GV(Tag, PL) == Value end end,
    Pick = fun(A,B,Op) -> case erlang:Op(A, B) of true -> A; _ -> B end end,
    LabelMatch = fun(Cur) -> Filter(?LABEL, <<Cur/binary, "/BTC">>) end,

    Markets = get_markets(),
    Info = get_info(),

    {[{<<"markets">>, {MarketData1}}]} = get_general_market_data(),
    MarketData = lists:map(fun({_Label, {L}}) -> L end, MarketData1),


    {[{<<"balances_available">>,{Balances0}} | _]} = Info,
    Balances = lists:filter(fun({_, FB}) -> 0 /= binary_to_float(FB) end,
                            Balances0),

    BTCValue =
        fun({<<"BTC">>, FB}) ->
                binary_to_float(FB);
           ({Cur, FB}) ->
                [PL] = lists:filter(LabelMatch(Cur), MarketData),
                LTP = ?GV(?LASTTRADEPRICE, PL),
                binary_to_float(FB) *  binary_to_float(LTP)
        end,
    _TotalBTC = lists:sum(lists:map(BTCValue, Balances)),

    AltsAcc = lists:map(fun({Cur, _}) ->
                                [PL] = lists:filter(LabelMatch(Cur), Markets),
                                {?GV(?MARKETID, PL), Cur, []}
                        end, lists:keydelete(<<"BTC">>, 1, Balances)),

    AltsTrades = lists:map(fun({MId, _, _}) ->
                                   M = binary:bin_to_list(MId),
                                   get_my_trades(M)
                           end, AltsAcc),
    %% In order to calculate average buy price over all currently held coins,
    %% we add all buys and for each sell, negate equal quantity of previous buys
    %% TODO: Does this make sense from a statistics point of view?
    NegateBuys =
        fun({PL}, {MId, Cur, Buys}) ->
                G = fun(Tag) -> ?GV(Tag, PL) end,
                TP  = binary_to_float(G(?TRADEPRICE)),
                Q   = binary_to_float(G(?QUANTITY)),
                TT  = G(?TRADETYPE),
                NegateBuy =
                    fun(Buy, 0) -> {Buy, 0};
                       ({BTP, BQ}, SellQLeft) when BQ < SellQLeft ->
                            {{BTP, 0}, SellQLeft - BQ};
                       ({BTP, BQ}, SellQLeft) ->
                            {{BTP, BQ - SellQLeft}, 0}
                    end,
                case TT of
                    ?TRADETYPE_BUY -> {MId, Cur, [{TP, Q} | Buys]};
                    ?TRADETYPE_SELL ->
                        {NewBuys, _} = lists:mapfoldl(NegateBuy, Q, Buys),
                        {MId, Cur, NewBuys}
                end
        end,
    NegatedBuys = lists:map(fun({AltAcc, AltTrades}) ->
                                    lists:foldl(NegateBuys, AltAcc, AltTrades)
                            end, lists:zip(AltsAcc, AltsTrades)),
    Sum = fun({P, Q}, {AltQ, BTCQ}) -> {AltQ + Q, BTCQ + (P * Q)} end,
    Averages = lists:map(fun({_, Cur, Buys}) ->
                                 {AltQ, BTCQ} = lists:foldl(Sum, {0,0}, Buys),
                                 case AltQ == 0 of
                                     true ->
                                         {Cur, AltQ, BTCQ, 0};
                                     false ->
                                         {Cur, AltQ, BTCQ, BTCQ / AltQ}
                                 end
                         end, NegatedBuys),
    ?info("~p", [Averages]),
    %MyTrades = get_my_trades(),
    %ca_visuals:pretty_print_my_trades(MyTrades, MarketData, Info),
    ok.

get_info() ->
    get_json({"getinfo", ?MY_TRADES_DATA_MAX_AGE},
             {auth_endpoint, "method=getinfo"}).

get_my_trades(MId) ->
    POST = "method=mytrades&marketid=" ++ MId ++ "&limit=1000",
    get_json({"mytrades-" ++ MId, ?MY_TRADES_DATA_MAX_AGE},
             {auth_endpoint, POST}).

get_general_market_data() ->
    get_json({"general-market-data", ?MY_TRADES_DATA_MAX_AGE},
             {public_endpoint, "method=marketdatav2"}).

get_json({FilePrefix, MaxAge}, {EndpointType, POSTData}) ->
    case market_files(FilePrefix, MaxAge) of
        none ->
            case update_data(FilePrefix,
                             fun() -> curl(EndpointType, POSTData) end) of
                {error, _} = E -> E;
                FN -> json_data(FN)
            end;
        FN -> json_data(FN)
    end.

json_data(FN) ->
    {ok, Bin} = file:read_file(FN),
    JSON = jiffy:decode(Bin),
    {[{<<"success">>, _Success},
      {<<"return">>, PL}]} = JSON,
    PL.

curl(public_endpoint, POSTData) ->
    {ok, Endpoint} = application:get_env(?APP, public_endpoint),
    Template = "curl --silent --data \"~s\" ~s",
    CMD = lists:flatten(io_lib:format(Template, [POSTData, Endpoint])),
    Res = time(fun() -> os:cmd(CMD) end, "Curl Post"),
    Res;
curl(auth_endpoint, POSTData0) ->
    {ok, Endpoint} = application:get_env(?APP, auth_endpoint),
    {ok, PublicKeyHexStr} = application:get_env(?APP, public_api_key),
    {ok, SecretKeyHexStr} = application:get_env(?APP, secret_api_key),
    POSTData = POSTData0 ++ "&nonce=" ++ new_nonce(),
    Sign = crypto:hmac(sha512, SecretKeyHexStr, POSTData),
    SignHex = string:to_lower(binary:bin_to_list(bin_to_hex(Sign))),
    Headers =
        [{"Sign", SignHex},
         {"Key", PublicKeyHexStr},
         {"Content-Type", "application/x-www-form-urlencoded"},
         {"Accept","*/*"},
         {"Content-Length", integer_to_list(length(POSTData))}
        ],
    HeaderStr =
        fun({HeaderFieldName, Value}) -> "--header \"" ++ HeaderFieldName ++
                                             ": " ++ Value ++ "\" "
        end,
    FinalHeader = lists:flatten(lists:map(HeaderStr, Headers)),
    Template = "curl --silent " ++ FinalHeader ++ "--data \"~s\" ~s",
    Params = [POSTData, Endpoint],
    CMD = lists:flatten(io_lib:format(Template, Params)),
    ?info("Curl: ~s", [CMD]),
    ?info("Please wait, this can take a few minutes...", []),
    Res = time(fun() -> os:cmd(CMD) end, "Curl Post"),
    ?info("Curl request response received. Parsing...", []),
    Res.

market_files(Prefix, MaxAge) ->
    {ok, FNs} = file:list_dir(?MARKET_DATA_DIR),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    CheckAge =
        fun(FN, Acc) ->
                AbsFN = filename:join(?MARKET_DATA_DIR, FN),
                case string:tokens(FN, "_") of
                    [Prefix, GregSecs] ->
                        case (Age = (Now - list_to_integer(GregSecs))) >
                            MaxAge of
                            true ->  ok = file:delete(AbsFN),
                                     Acc;
                            false ->
                                ?info("~s is ~p seconds old, using it...",
                                      [Prefix, Age]),
                                AbsFN
                        end;
                    _ -> Acc
                end
        end,
    lists:foldl(CheckAge, none, FNs).

update_data(FilePrefix, CurlFun) ->
    ?info("Market data for ~p outdated, fetching live data...", [FilePrefix]),
    try
        LiveData = CurlFun(),
        JSON = jiffy:decode(LiveData),
        {[{<<"success">>, Success},
          {<<"return">>, PL}]} = JSON,
        case ((Success == 1) or (Success == <<"1">>)) of
            true -> continue;
            false ->
                ?error("Call return is invalid for ~p: ~p",
                       [FilePrefix, Success]),
                throw(call_did_not_return_valid_json)
        end,
        NowSecs = calendar:datetime_to_gregorian_seconds(
                    calendar:universal_time()),
        S = io_lib:format("~s_~p", [to_list(FilePrefix), NowSecs]),
        FN = filename:join(?MARKET_DATA_DIR, lists:flatten(S)),
        ok = file:write_file(FN, LiveData),
        FN
    catch E:R ->
            ?error("Fetching market data failed: ~p",
                   [{E,R,erlang:get_stacktrace()}]),
            {error, fetching_market_data_failed}
    end.

new_nonce() ->
    {MegaSeconds, Seconds, MicroSeconds} = now(),
    %%% return microseconds; now() is guaranteed to continously increase
    MS = integer_to_list(MegaSeconds * 1000000000000 +
                             Seconds * 1000000 +
                             MicroSeconds),
    string:sub_string(MS, 1, 10).

bin_to_hex(Bin) ->
    <<<<(case byte_size(H = integer_to_binary(X,16)) of
             1 -> <<"0", H/binary>>;
             _ -> H
         end)/binary>> || <<X>> <= Bin>>.

hex_to_bin(Hex) when is_list(Hex) -> hex_to_bin(binary:list_to_bin(Hex));
hex_to_bin(Hex) ->
    <<<<(binary_to_integer(<<B1,B2>>, 16))>> || <<B1,B2>> <= Hex>>.

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary:bin_to_list(B);
to_list(A) when is_atom(A) -> erlang:atom_to_list(A);
to_list(I) when is_integer(I) -> erlang:integer_to_list(I).

time(OpFun, OpName) ->
    {Time, Res} = timer:tc(OpFun),
    ?info("~s took ~p ms.", [OpName, Time div 1000]),
    Res.

get_markets() ->
    {ok, [CryptsyMarkets]} = file:consult(?MARKETS_FILE),
    lists:map(fun({_Label, PL}) -> PL end, CryptsyMarkets).
