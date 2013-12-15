-define(info(Str), lager:info(Str, [])).
-define(info(Str, Args), lager:info(Str, Args)).

-define(warning(Str), lager:warning(Str, [])).
-define(warning(Str, Args), lager:warning(Str, Args)).

-define(error(Str), lager:error(Str, [])).
-define(error(Str, Args), lager:error(Str, Args)).

-define(APP, caerus).

-define(INTERNAL_FILE(Name), filename:join(ca_util:priv_dir(), Name)).
-define(MARKETS_FILE,
        ?INTERNAL_FILE("markets")).

-define(MARKET_DATA_DIR,
        ?INTERNAL_FILE("market_data/")).

-define(MY_TRADES_DATA_MAX_AGE, 86400). %% Seconds

-define(GV(Key, PL), proplists:get_value(Key, PL)).


%% marketdatav2
-define(MARKETID,       <<"marketid">>).
-define(LABEL,          <<"label">>).
-define(LASTTRADEPRICE, <<"lasttradeprice">>).
-define(VOLUME,         <<"volume">>).
-define(LASTTRADETIME,  <<"lasttradetime">>).
-define(PRIMARYNAME,    <<"primaryname">>).
-define(PRIMARYCODE,    <<"primarycode">>).
-define(SECONDARYNAME,  <<"secondaryname">>).
-define(SECONDARYCODE,  <<"secondarycode">>).
-define(RECENTTRADES,   <<"recenttrades">>).
-define(SELLORDERS,     <<"sellorders">>).
-define(BUYORDERS,      <<"buyorders">>).

%% allmytrades
-define(TRADEID,            <<"tradeid">>).
-define(TRADETYPE,          <<"tradetype">>).
-define(TRADETYPE_BUY,      <<"Buy">>).
-define(TRADETYPE_SELL,     <<"Sell">>).
-define(DATETIME,           <<"datetime">>).
-define(TRADEPRICE,         <<"tradeprice">>).
-define(QUANTITY,           <<"quantity">>).
-define(FEE,                <<"fee">>).
-define(TOTAL,              <<"total">>).
-define(INITIATE_ORDERTYPE, <<"initiate_ordertype">>).
-define(ORDER_ID,           <<"order_id">>).

%% internal
-define(SECS,              <<"seconds">>).
-define(X,                 <<"x">>).
-define(Y,                 <<"y">>).
