%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Event handler implementation.
%%% @end
%%% Created : 13. May 2019 19:46
%%%-------------------------------------------------------------------
-module(wms_events_actor).
-author("Attila Makra").

-include_lib("wms_common/include/wms_common.hrl").
-include_lib("wms_logger/include/wms_logger.hrl").

-type state() :: #{}.

%% API
-export([subscribe/4,
         fire_event/3,
         init/0]).

-spec init() ->
  map().
init() ->
  {ok, Pid} = wms_events_publisher:start_link(),
  #{pid => Pid}.

%% =============================================================================
%% Actor's API functions
%% =============================================================================

-spec subscribe(state(), wms_common:timestamp(), binary(), binary()) ->
  {state(), ok}.
subscribe(State, Timestamp, EventID, TaskInstanceID) ->
  ok = wms_db:add_subscriber(Timestamp, EventID, TaskInstanceID),
  {State, ok}.

-spec fire_event(state(), wms_common:timestamp(), binary()) ->
  {state(), ok}.
fire_event(State, Timestamp, EventID) ->
  wms_events_publisher:fire_event(Timestamp, EventID),
  {State, ok}.