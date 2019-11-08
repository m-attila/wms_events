%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2019 18:45
%%%-------------------------------------------------------------------
-module(wms_events_SUITE).
-author("Attila Makra").

-compile(nowarn_export_all).
-compile(export_all).

-include("wms_events.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
  [{key, value}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  [{key, value} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
  ?MODULE:GroupName({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(GroupName, Config) ->
  ?MODULE:GroupName({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
  [
    {pubsub_group,
     [{repeat_until_any_fail, 1}],
     [
       publish_subscribe_test,
       mandatory_publish_subscribe_test
     ]
    }
  ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
  [
    {group, pubsub_group}
  ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------

%% =============================================================================
%% Table create group
%% =============================================================================

pubsub_group({prelude, Config}) ->
  SaveMode = os:getenv("wms_mode"),
  os:putenv("wms_mode", "test"),
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  wms_dist:call(wms_events_actor, module_info, []),
  [{started, StartedApps}, {save_mode, SaveMode} | Config];
pubsub_group({postlude, Config}) ->
  StartedApps = ?config(started, Config),
  [application:stop(App) || App <- StartedApps],
  os:putenv("wms_mode", ?config(save_mode, Config)),
  ok.

%%--------------------------------------------------------------------
%% Initialization test
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Subscribe for events, fire event test
%%
%%--------------------------------------------------------------------

%% test case information
publish_subscribe_test({info, _Config}) ->
  [""];
publish_subscribe_test(suite) ->
  ok;
%% init test case
publish_subscribe_test({prelude, Config}) ->
  clear_tables(),
  test_wms_engine_actor = ets:new(test_wms_engine_actor,
                                  [set, public, named_table, {keypos, 1}]),
  ets:insert(test_wms_engine_actor, {notify, []}),

  Module = wms_events_publisher:get_notification_module(),
  wms_events_publisher:set_notification_module(test_wms_engine_actor),

  [{module, Module} | Config];
%% destroy test case
publish_subscribe_test({postlude, Config}) ->
  Module = ?config(module, Config),
  wms_events_publisher:set_notification_module(Module),
  ets:delete(test_wms_engine_actor),
  ok;
%% test case implementation
publish_subscribe_test(_Config) ->
  EventID = <<"event1">>,

  TsSub1 = wms_common:timestamp(),
  Task1 = <<"task1">>,

  TsSub2 = wms_common:timestamp(),
  Task2 = <<"task2">>,

  wms_events:subscribe(TsSub1, EventID, Task1),
  wms_events:subscribe(TsSub2, EventID, Task2),

  TsEvent = wms_common:timestamp(),
  ?assertEqual(2, length(wms_db:get_subscribers(TsEvent, EventID))),

  timer:sleep(100),

  % fire event
  wms_events:fire_event(TsEvent, EventID),
  timer:sleep(500),

  [{notify, Notifies}] = ets:lookup(test_wms_engine_actor, notify),
  ?assertEqual([{EventID, Task1},
                {EventID, Task2}], Notifies),

  ?assertEqual([], wms_db:get_subscribers(TsEvent, EventID)),
  ?assertEqual([], wms_db:get_event(EventID)),
  ok.

%%--------------------------------------------------------------------
%% Publish-subscribe test for mandatory event
%%
%%--------------------------------------------------------------------

%% test case information
mandatory_publish_subscribe_test({info, _Config}) ->
  [""];
mandatory_publish_subscribe_test(suite) ->
  ok;
%% init test case
mandatory_publish_subscribe_test({prelude, Config}) ->
  clear_tables(),
  test_wms_engine_actor = ets:new(test_wms_engine_actor,
                                  [set, public, named_table, {keypos, 1}]),
  ets:insert(test_wms_engine_actor, {notify, []}),

  Module = wms_events_publisher:get_notification_module(),
  wms_events_publisher:set_notification_module(test_wms_engine_actor),
  [{module, Module} | Config];
%% destroy test case
mandatory_publish_subscribe_test({postlude, Config}) ->
  Module = ?config(module, Config),
  wms_events_publisher:set_notification_module(Module),

  ets:delete(test_wms_engine_actor),
  ok;
%% test case implementation
mandatory_publish_subscribe_test(_Config) ->
  EventID = <<"event1">>,

  % fire event first
  TsEvent = wms_common:timestamp(),
  wms_events:fire_event(TsEvent, {mandatory, EventID}),
  timer:sleep(500),

  TsSub1 = wms_common:timestamp(),
  Task1 = <<"task1">>,

  wms_events:subscribe(TsSub1, EventID, Task1),

  % wait for notification
  timer:sleep(500),

  [{notify, Notifies}] = ets:lookup(test_wms_engine_actor, notify),
  ?assertEqual([{EventID, Task1}], Notifies),

  ?assertEqual([], wms_db:get_subscribers(TsEvent, EventID)),
  ?assertEqual([], wms_db:get_event(EventID)),
  ok.

% Clear db tables from mnesia
clear_tables() ->
  [{atomic, ok} = mnesia:clear_table(Table) ||
    Table <- mnesia:system_info(tables),
   Table =/= schema].
