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
%%%============================================================================
%%% Internal functions
%%%============================================================================
secs_since_last_trade(LastTradeTime) ->
    now_secs() - (3600 * 5) -
        calendar:datetime_to_gregorian_seconds(
          ca_util:cryptsy_datetime_to_calendar_datetime(LastTradeTime)).

now_secs() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
