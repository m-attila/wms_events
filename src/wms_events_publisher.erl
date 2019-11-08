%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Esemeny kiertesiteset vegzi el
%%% @end
%%% Created : 07. Nov 2019 12:30
%%%-------------------------------------------------------------------
-module(wms_events_publisher).
-author("Attila Makra").
-behavior(gen_server).

-include_lib("wms_logger/include/wms_logger.hrl").
-include_lib("wms_common/include/wms_common.hrl").

%% API
-export([start_link/0,
         fire_event/2,
         set_notification_module/1,
         get_notification_module/0]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3]).

-record(state, {
  notification_module = wms_engine_actor :: module(),
  mandatory_events :: [{timestamp(), EventID :: binary()}],
  normal_events :: [{timestamp(), EventID :: binary()}]
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  ?info("stared."),
  Ret.

-spec fire_event(timestamp(), binary() | {mandatory, binary()}) ->
  any().
fire_event(Timestamp, EventID) ->
  gen_server:call(?MODULE, {fire_event, Timestamp, EventID}).

-spec set_notification_module(module()) ->
  ok.
set_notification_module(Module) ->
  gen_server:call(?MODULE, {set_notification_module, Module}).

-spec get_notification_module() ->
  module().
get_notification_module() ->
  gen_server:call(?MODULE, get_notification_module).

%% =============================================================================
%% gen_server behaviour
%% =============================================================================

-spec init(Args :: term()) ->
  {ok, State :: state()}.
init(_) ->
  {ok, #state{
    mandatory_events = [],
    normal_events    = []}}.

-spec handle_info(Info :: any(), State :: state()) ->
  {noreply, State :: state()}.

handle_info(notify_not_mandatory, #state{normal_events       = [LastEvent],
                                         notification_module = Module} = State) ->
  notify_not_mandatory(Module, LastEvent),
  {noreply, State#state{normal_events = []}};

handle_info(notify_not_mandatory, #state{normal_events       =
                                         [LastEvent | RestEvents],
                                         notification_module = Module} = State) ->
  notify_not_mandatory(Module, LastEvent),
  self() ! notify_not_mandatory,
  {noreply, State#state{normal_events = RestEvents}};

handle_info(notify_mandatory, #state{mandatory_events    = [LastEvent],
                                     notification_module = Module} = State) ->
  NewState =
    case notify_mandatory(Module, LastEvent) of
      true ->
        State#state{mandatory_events = []};
      false ->
        erlang:send_after(10, self(), notify_mandatory),
        State
    end,
  {noreply, NewState};

handle_info(notify_mandatory, #state{mandatory_events    =
                                     [LastEvent | RestEvent],
                                     notification_module = Module} = State) ->
  NewState =
    case notify_mandatory(Module, LastEvent) of
      true ->
        self() ! notify_mandatory,
        State#state{mandatory_events = RestEvent};
      false ->
        erlang:send_after(10, self(), notify_mandatory),
        State#state{mandatory_events = RestEvent ++ [LastEvent]}
    end,
  {noreply, NewState};

handle_info(Msg, State) ->
  ?warning("Unknown message: ~0p", [Msg]),
  {noreply, State}.

-spec handle_call(Info :: any(), From :: {pid(), term()}, State :: state()) ->
  {reply, term(), State :: state()}.
handle_call({fire_event, Timestamp, {mandatory, EventID} = EventSpec}, _From,
            #state{mandatory_events = MandatoryEvents} = State) ->
  % store event in database
  ok = wms_db:add_event(Timestamp, EventSpec),

  self() ! notify_mandatory,
  {reply, ok, State#state{
    mandatory_events = MandatoryEvents ++ [{Timestamp, EventID}]}};

handle_call({fire_event, Timestamp, EventID}, _From,
            #state{normal_events = NormalEvents} = State) ->
  % store event in database
  ok = wms_db:add_event(Timestamp, EventID),

  self() ! notify_not_mandatory,
  {reply, ok, State#state{
    normal_events = NormalEvents ++ [{Timestamp, EventID}]}};

handle_call({set_notification_module, Module}, _From, State) ->
  {reply, ok, State#state{notification_module = Module}};
handle_call(get_notification_module, _From,
            #state{notification_module = Module} = State) ->
  {reply, Module, State}.

-spec handle_cast(Request :: any(), State :: state()) ->
  {noreply, State :: state()}.
handle_cast(_, State) ->
  {noreply, State}.

%% =============================================================================
%% Private functions
%% =============================================================================

-spec notify_not_mandatory(module(), {timestamp(), binary()}) ->
  term().
notify_not_mandatory(Module, {Timestamp, EventID}) ->
  SubScribers = wms_db:get_subscribers(Timestamp, EventID),
  publish(Module, SubScribers),
  {ok, _} = wms_db:remove_event(Timestamp, EventID).

-spec notify_mandatory(module(), {timestamp(), binary()}) ->
  boolean().
notify_mandatory(Module, {Timestamp, EventID}) ->
  SubScribers = wms_db:get_subscribers(dont_care, EventID),
  case SubScribers of
    [] ->
      % no subscriber yet
      false;
    _ ->
      publish(Module, SubScribers),
      {ok, _} = wms_db:remove_event(Timestamp, EventID),
      true
  end.

-spec publish(module(), [{SubscriberTimestamp :: wms_common:timestamp(),
                          EventID :: binary(),
                          TaskInstanceID :: binary()}]) ->
               ok.
publish(_, []) ->
  ok;
publish(Module, [{Timestamp, EventID, TaskInstanceID} | RestSubscribers]) ->
  % notify subscribed task
  ok = wms_dist:call(Module,
                     notify,
                     [EventID, TaskInstanceID]),
  {ok, _} = wms_db:remove_subscriber(Timestamp,
                                     EventID,
                                     TaskInstanceID),
  publish(Module, RestSubscribers).

