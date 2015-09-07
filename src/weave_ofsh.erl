-module(weave_ofsh).

-behaviour(ofs_handler).

%% API
-export(
   [is_connected/1,
    all_connected/0]).

%% ofs_handler callbacks
-export([
    init/7,
    connect/8,
    disconnect/1,
    %% We don't need handle_message/2, since we don't subscribe
    %% to unsolicited messages.
    %% handle_message/2,
    handle_error/2,
    terminate/1
]).

-spec is_connected(binary() | string()) -> boolean().
is_connected(DatapathId) when is_binary(DatapathId) ->
    is_connected(binary_to_list(DatapathId));
is_connected(DatapathId) when is_list(DatapathId) ->
    ets:member(weave_switch_connections, DatapathId).

-spec all_connected() -> [string()].
all_connected() ->
    ets:select(weave_switch_connections, [{{'$1', '_'}, [], ['$1']}]).

init(_Mode, IpAddress, DatapathId, _Features, _Version, Connection, _Options)
  when is_list(DatapathId) ->
    lager:info("Connection from switch at ~s, datapath id ~s",
               [inet:ntoa(IpAddress), DatapathId]),
    ets:insert(weave_switch_connections, {DatapathId, Connection}),
    {ok, #{datapath_id => DatapathId}}.

connect(_Mode, _IpAddress, _DatapathId, _Features, _Version, _Connection, _AuxId, _Opts) ->
    {error, auxiliary_connection_not_supported}.

disconnect(_State) ->
    ok.

handle_error(Reason, #{datapath_id := DatapathId}) ->
    lager:info("Connection error for ~s: ~p", [DatapathId, Reason]),
    ets:delete(weave_switch_connections, DatapathId),
    ok.

terminate(#{datapath_id := DatapathId}) ->
    lager:info("Connection terminated for ~s", [DatapathId]),
    ets:delete(weave_switch_connections, DatapathId),
    ok.
