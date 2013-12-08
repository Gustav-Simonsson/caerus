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

-define(MARKET_DATA_MAX_AGE, 900). %% Seconds

-define(GV(Key, PL), proplists:get_value(Key, PL)).

-define(MARKET_IDS,
        [
         {70, "CGB", "BTC"}
        ]).

-define(MARKET_MARKETID, <<"marketid">>).
-define(MARKET_LABEL, <<"label">>).
-define(MARKET_LASTTRADEPRICE, <<"lasttradeprice">>).
-define(MARKET_VOLUME, <<"volume">>).
-define(MARKET_LASTTRADETIME, <<"lasttradetime">>).
-define(MARKET_PRIMARYNAME,  <<"primaryname">>).
-define(MARKET_PRIMARYCODE, <<"primarycode">>).
-define(MARKET_SECONDARYNAME, <<"secondaryname">>).
-define(MARKET_SECONDARYCODE, <<"secondarycode">>).
-define(MARKET_RECENTTRADES, <<"recenttrades">>).
-define(MARKET_SELLORDERS, <<"sellorders">>).
-define(MARKET_BUYORDERS, <<"buyorders">>).
