-module(ca_app).

-behaviour(application).

%% Application callbacks
-export([start/0, restart/0, stop/0,
         start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start() ->
    application:ensure_started(lager),
    application:ensure_started(crypto),
    application:ensure_started(caerus),
    ok.

restart() ->
    application:stop(caerus),
    start().

stop() ->
    application:stop(caerus).

start(_StartType, _StartArgs) ->
    ca_sup:start_link().

stop(_State) ->
    ok.
