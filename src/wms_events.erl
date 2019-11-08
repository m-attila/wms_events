%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2019 14:16
%%%-------------------------------------------------------------------
-module(wms_events).
-author("Attila Makra").

-include("wms_events.hrl").
-include_lib("wms_common/include/wms_common.hrl").

%% API

-spec subscribe(timestamp(), binary(), binary()) ->
  ok.
-export([subscribe/3,
         fire_event/2]).

subscribe(Timestamp, EventID, TaskInstanceID) ->
  wms_dist:call(wms_events_actor,
                subscribe,
                [Timestamp, EventID, TaskInstanceID]).
-spec fire_event(timestamp(), binary()) ->
  ok.
fire_event(Timestamp, EventID) ->
  wms_dist:call(wms_events_actor,
                fire_event,
                [Timestamp, EventID]).


%% =============================================================================
%% Private functions
%% =============================================================================
