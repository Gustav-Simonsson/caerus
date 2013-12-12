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

cryptsy_datetime_to_calendar_datetime(Datetime) ->
    [Y,M,D,H,Min,S] =
        lists:map(fun erlang:list_to_integer/1,
                  string:tokens(binary:bin_to_list(Datetime), "-: ")),
    {{Y,M,D}, {H,Min,S}}.

cryptsy_datetime_to_month_day(Datetime) ->
    <<_:5/binary, MD:5/binary, _/binary>> = Datetime,
    MD.

%%%============================================================================
%%% Internal functions
%%%============================================================================
strip_dynamic_data_from_market({Label, {PL}}) ->
    DynamicTags =
        [?LASTTRADEPRICE,
         ?VOLUME,
         ?LASTTRADETIME,
         ?RECENTTRADES,
         ?SELLORDERS,
         ?BUYORDERS
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
