%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than
%%% one rr_balancer. This is by design, not by accident.
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(robot).

-behaviour(gen_statem).

%% API
-export([start/2, start_link/2, stop/1, next/1]).
%% Supervisor Callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
%% State Callbacks
-export([comp_up_open/3, comp_down_open/3, comp_down_close/3, comp_up_close/3,
         box_up_close/3, box_down_close/3, box_down_open/3, box_up_open/3]).

%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts the state machine locally with the passed in registered name
%% , sets the initial state, and does not link to the caller.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(), term()) -> {ok, atom()}.
start(Statem_name, Initial_state) ->
  gen_statem:start({local, Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts the state machine locally with the passed in registered name
%% , sets the initial state, and does link to the caller.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), term()) -> {ok, atom()}.
start_link(Statem_name, Initial_state) ->
  gen_statem:start_link({local, Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
  gen_statem:stop(Statem_name).

next(Statem_name) ->
  gen_statem:call(Statem_name, next).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
  void.

%% @private
code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

%% @private
init([]) ->
  %% Set the initial state + data.  Data is used only as a counter.
  {ok, comp_up_open, 0}.

%% @private
callback_mode() ->
  state_functions.

%%% state callback(s)
comp_up_open({call, From}, next, Data) ->
  {next_state, comp_down_open, Data, [{reply, From, comp_down_open}]};
comp_up_open(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

comp_down_open({call, From}, next, Data) ->
  {next_state, comp_down_close, Data, [{reply, From, comp_down_close}]};
comp_down_open(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

comp_down_close({call, From}, next, Data) ->
  {next_state, comp_up_close, Data, [{reply, From, comp_up_close}]};
comp_down_close(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

comp_up_close({call, From}, next, Data) ->
  {next_state, box_up_close, Data, [{reply, From, box_up_close}]};
comp_up_close(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

box_up_close({call, From}, next, Data) ->
  {next_state, box_down_close, Data, [{reply, From, box_down_close}]};
box_up_close(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

box_down_close({call, From}, next, Data) ->
  {next_state, box_down_open, Data, [{reply, From, box_down_open}]};
box_down_close(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

box_down_open({call, From}, next, Data) ->
  {next_state, box_up_open, Data, [{reply, From, box_up_open}]};
box_down_open(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

box_up_open({call, From}, next, Data) ->
  {next_state, comp_up_open, Data, [{reply, From, comp_up_open}]};
box_up_open(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% @private
handle_event(_, _, Data) ->
  %% Ignore all other events
  {keep_state, Data}.
