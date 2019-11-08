%%%-------------------------------------------------------------------
%% @doc wms_events public API
%% @end
%%%-------------------------------------------------------------------

-module(wms_events_app).

-behaviour(application).

-include("wms_events.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-spec start(Type :: application:start_type(), Args :: term()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    ok = wms_cfg:start_apps(?APP_NAME, [wms_dist, wms_db]),
    wms_events_sup:start_link().
%%--------------------------------------------------------------------
-spec stop(any()) ->
    atom().
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
