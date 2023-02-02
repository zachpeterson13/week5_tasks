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
-export([start/2, start_link/2, stop/1, to_up/1, to_down/1, to_close/1, to_open/1,
         to_box/1, to_comp/1, get_state/1]).
%% Supervisor Callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).

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

to_up(Statem_name) ->
  gen_statem:call(Statem_name, to_up).

to_down(Statem_name) ->
  gen_statem:call(Statem_name, to_down).

to_close(Statem_name) ->
  gen_statem:call(Statem_name, to_close).

to_open(Statem_name) ->
  gen_statem:call(Statem_name, to_open).

to_comp(Statem_name) ->
  gen_statem:call(Statem_name, to_comp).

to_box(Statem_name) ->
  gen_statem:call(Statem_name, to_box).

get_state(Statem_name) ->
  gen_statem:call(Statem_name, get_state).

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
  handle_event_function.

%%% state callback(s)
handle_event({call, From}, to_down, comp_up_open, Data) ->
  {next_state, comp_down_open, Data, [{reply, From, comp_down_open}]};
handle_event({call, From}, to_close, comp_down_open, Data) ->
  {next_state, comp_down_close, Data, [{reply, From, comp_down_close}]};
handle_event({call, From}, to_up, comp_down_close, Data) ->
  {next_state, comp_up_close, Data, [{reply, From, comp_up_close}]};
handle_event({call, From}, to_box, comp_up_close, Data) ->
  {next_state, box_up_close, Data, [{reply, From, box_up_close}]};
handle_event({call, From}, to_down, box_up_close, Data) ->
  {next_state, box_down_close, Data, [{reply, From, box_down_close}]};
handle_event({call, From}, to_open, box_down_close, Data) ->
  {next_state, box_down_open, Data, [{reply, From, box_down_open}]};
handle_event({call, From}, to_up, box_down_open, Data) ->
  {next_state, box_up_open, Data, [{reply, From, box_up_open}]};
handle_event({call, From}, to_comp, box_up_open, Data) ->
  {next_state, comp_up_open, Data, [{reply, From, comp_up_open}]};
handle_event({call, From}, get_state, State, Data) ->
  %% Reply with the current state
  {next_state, State, Data, [{reply, From, State}]};
handle_event({call, From}, _, State, Data) ->
  %% Ignore all other events
  {next_state, State, Data, [{reply, From, ignored_event}]}.
