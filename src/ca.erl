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

-define(CURL_TIMEOUT, 5000).

-record(s, {markets = []
           }).

%%%============================================================================
%%% API
%%%============================================================================
view_market(MId) ->
    case is_list(MId) of
        false ->   {error, market_id_must_be_string};
        true ->
            gen_server:call(?SERVER, {view_market, MId}, ?CURL_TIMEOUT + 100)
    end.

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

handle_call({view_market, CurName}, _From, #s{markets = Markets} = S) ->
    do_view_market(CurName, Markets),
    {reply, ok, S};
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
do_view_market(CurName, Markets) ->
    MId = market_cur_name_to_int(CurName, Markets),
    {ok, FNs} = file:list_dir(?MARKET_DATA_DIR),
    MarketData = get_single_market_data(MId, FNs),
    ca_visuals:pretty_print_single_market_data(MarketData).

do_view_my_trades() ->
    {ok, FNs} = file:list_dir(?MARKET_DATA_DIR),
    MyTrades = get_my_trades(FNs),
    %ca_visuals:pretty_print_my_trades(MyTrades).
    ok.

get_my_trades(FileNames) ->
    FilePrefix = "my_trades",
    case market_files(FilePrefix, FileNames) of
        none ->
            case update_data(FilePrefix, fun() -> curl_all_my_trades() end) of
                {error, _} = E -> E;
                FN -> my_trades_data(FN)
            end;
        FN -> my_trades_data(FN)
    end.

get_single_market_data(MIdBin, FileNames) ->
    MId = binary:bin_to_list(MIdBin),
    case market_files(MId, FileNames) of
        none ->
            case update_data(MId, fun() -> curl_single_market_data(MId) end) of
                {error, _} = E -> E;
                FN -> single_market_data(FN)
            end;
        FN -> single_market_data(FN)
    end.

single_market_data(FN) ->
    {ok, Bin} = file:read_file(FN),
    JSON = jiffy:decode(Bin),
    {[{<<"success">>,1},
      {<<"return">>,
       {[{<<"markets">>,
          {[{_Label,
             {PL}}]}}]}}]} = JSON,
    PL.

my_trades_data(FN) ->
    {ok, Bin} = file:read_file(FN),
    JSON = jiffy:decode(Bin),
    {[{<<"success">>,1},
      {<<"return">>,
       {[{<<"markets">>,
          {[{_Label,
             {PL}}]}}]}}]} = JSON,
    PL.

curl_single_market_data(MId) ->
    Format = io_lib:format("method=singlemarketdata&marketid=~s", [MId]),
    POSTData = lists:flatten(Format),
    curl(public_endpoint, POSTData).

curl_all_my_trades() ->
    POSTData = "method=getinfo",
    curl(auth_endpoint, POSTData).

curl(public_endpoint, POSTData) ->
    {ok, Endpoint} = application:get_env(?APP, public_endpoint),
    Template = "curl --silent --data \"~s\" ~s",
    CMD = lists:flatten(io_lib:format(Template, [POSTData, Endpoint])),
    os:cmd(CMD);
curl(auth_endpoint, POSTData0) ->
    {ok, Endpoint} = application:get_env(?APP, auth_endpoint),
    {ok, PublicKeyHexStr} = application:get_env(?APP, public_api_key),
    {ok, SecretKeyHexStr} = application:get_env(?APP, secret_api_key),
    %PublicKey = hex_to_bin(PublicKeyHexStr),
    SecretKey = hex_to_bin(SecretKeyHexStr),
    POSTData = POSTData0 ++ "&nonce=" ++ new_nonce(),
    Sign = crypto:hmac(sha512, SecretKey, POSTData),
    SignHex = string:to_lower(binary:bin_to_list(bin_to_hex(Sign))),
    Header1 = "Sign: " ++ SignHex,
    Header2 = " Key: " ++ PublicKeyHexStr,
    Template = "curl --silent --header \"~s\" --header \"~s\" --data \"~s\" ~s",
    Params = [Header1, Header2, POSTData, Endpoint],
    CMD = lists:flatten(io_lib:format(Template, Params)),
    ?info("HURR ~s", [CMD]),
    os:cmd(CMD).
    %ok.

market_cur_name_to_int(CurName, CryptsyMarkets) ->
    {match, L} = re:run(CurName, "[[:alpha:]]+", [{capture, all, list}, global]),
    UpperCaseAlphas = string:to_upper(lists:concat(lists:append(L))),
    IdAndName = fun({_, PL}) -> PC = ?GV(?MARKET_PRIMARYNAME, PL),
                                {?GV(?MARKET_MARKETID, PL),
                                 string:to_upper(binary:bin_to_list(PC))} end,
    IdsAndNames = lists:map(IdAndName, CryptsyMarkets),
    fuzzy_search(UpperCaseAlphas, IdsAndNames).

fuzzy_search(Chars, IdsAndNames) ->
    %% return first currency name matching a substr; start with longest substrs
    SubStrs = lists:sort(fun(A,B) -> length(A) > length(B) end,
                         substrings(Chars)),
    SubStrMatch = fun(S) -> substring_match(S, IdsAndNames) end,
    NoMatch = fun(S) -> [] == SubStrMatch(S) end,
    case lists:dropwhile(NoMatch, SubStrs) of
        [SubStr | _] ->
            [{MId, _} | _] = SubStrMatch(SubStr),
            MId;
        [] ->
            not_found
    end.

substring_match(Str, IdsAndNames) ->
    NoMatch =
        fun({_MId, Name}) -> nomatch == re:run(Name, Str, [{capture, none}])
        end,
    lists:dropwhile(NoMatch, IdsAndNames).

substrings(S) ->
    Slen = length(S),
    [string:sub_string(S,B,E) ||
        B <- lists:seq(1, Slen), E <- lists:seq(B, Slen)].

market_files(Prefix, FileNames) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    CheckAge =
        fun(FN, Acc) ->
                AbsFN = filename:join(?MARKET_DATA_DIR, FN),
                case string:tokens(FN, "_") of
                    [Prefix, GregSecs] ->
                        case (Now - list_to_integer(GregSecs)) >
                            ?MARKET_DATA_MAX_AGE of
                            true ->  ok = file:delete(AbsFN),
                                     Acc;
                            false -> AbsFN
                        end;
                    _ -> Acc
                end
        end,
    lists:foldl(CheckAge, none, FileNames).

update_data(FilePrefix, CurlFun) ->
    ?info("Market data for ~p outdated, fetching live data...", [FilePrefix]),
    try
        LiveData = CurlFun(),
        {[{<<"success">>,1}, _]} = jiffy:decode(LiveData),
        NowSecs = calendar:datetime_to_gregorian_seconds(
                    calendar:universal_time()),
        S = io_lib:format("~p_~p", [list_to_integer(FilePrefix), NowSecs]),
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
    %string:sub_string(MS, 2, 11).
    MS.

bin_to_hex(Bin) ->
    <<<<(case byte_size(H = integer_to_binary(X,16)) of
             1 -> <<"0", H/binary>>;
             _ -> H
         end)/binary>> || <<X>> <= Bin>>.

hex_to_bin(Hex) when is_list(Hex) -> hex_to_bin(binary:list_to_bin(Hex));
hex_to_bin(Hex) ->
    <<<<(binary_to_integer(<<B1,B2>>, 16))>> || <<B1,B2>> <= Hex>>.
