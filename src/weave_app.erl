-module(weave_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(weave_switch_connections, [public, named_table]),
    weave_sup:start_link().

stop(_State) ->
    ok.
