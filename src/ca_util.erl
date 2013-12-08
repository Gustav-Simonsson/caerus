%%%----------------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%%     Misc utils
%%% @end
%%% Created : 04 Oct 2013 by Gustav Simonsson <gustav.simonsson@gmail.com>
%%%----------------------------------------------------------------------------
-module(ca_util).

-compile(export_all).

-include("ca.hrl").

%%%============================================================================
%%% API
%%%============================================================================
build_markets_include_file_from_dump(Bin) ->
    JSON = jiffy:decode(Bin),
    {[{<<"success">>,1},
      {<<"return">>,
       {[{<<"markets">>,
          {Markets}}]}}]} = JSON,
    StaticMarketsData =
        lists:map(fun strip_dynamic_data_from_market/1, Markets),
    ok = file:write_file(?MARKETS_FILE,
                         io_lib:format("~p.~n", [StaticMarketsData])),
    ok.
%%%============================================================================
%%% Internal functions
%%%============================================================================
strip_dynamic_data_from_market({Label, {PL}}) ->
    DynamicTags =
        [?MARKET_LASTTRADEPRICE,
         ?MARKET_VOLUME,
         ?MARKET_LASTTRADETIME,
         ?MARKET_RECENTTRADES,
         ?MARKET_SELLORDERS,
         ?MARKET_BUYORDERS
        ],
    Statics = [{Key, Value} || {Key, Value} <- PL,
                               not lists:member(Key, DynamicTags)],
    {Label, Statics}.

%% code:priv_dir/1 doesn't work if app is manually started (requires release)
%% or some specific upstart of the app
priv_dir() -> from_ebin("priv").

from_ebin(Dir) ->
    Ebin = filename:dirname(code:which(?APP)),
    filename:join(filename:dirname(Ebin), Dir).
