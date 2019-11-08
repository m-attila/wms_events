%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2019 20:27
%%%-------------------------------------------------------------------
-module(test_wms_engine_actor).
-author("Attila Makra").

%% API
-export([notify/3]).

notify(State, EventID, TaskInstanceID) ->
  [{notify, Notifies}] = ets:lookup(?MODULE, notify),
  ets:update_element(?MODULE,
                     notify,
                     {2, [{EventID, TaskInstanceID} | Notifies]}),
  {State, ok}.