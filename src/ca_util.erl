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

market_cur_name_to_int(CurName, CryptsyMarkets) ->
    {match, L} = re:run(CurName, "[[:alpha:]]+", [{capture, all, list}, global]),
    UpperCaseAlphas = string:to_upper(lists:concat(lists:append(L))),
    IdAndName = fun({_, PL}) -> PC = ?GV(?PRIMARYNAME, PL),
                                {?GV(?MARKETID, PL),
                                 string:to_upper(binary:bin_to_list(PC))} end,
    IdsAndNames = lists:map(IdAndName, CryptsyMarkets),
    fuzzy_search(UpperCaseAlphas, IdsAndNames).

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
