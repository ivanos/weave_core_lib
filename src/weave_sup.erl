-module(weave_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({}) ->
    %% Weave doesn't actually have any children.  This is an empty
    %% supervisor that's here just because we need an application
    %% callback module.
    {ok, {{one_for_one, 5, 10}, []}}.
