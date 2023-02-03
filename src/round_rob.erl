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
-module(round_rob).

-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is
%% being done.
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% API
-export([start/2, start_link/2, stop/1, get/1, add/2]).
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
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(), term()) -> {ok, atom()}.
start(Statem_name, Initial_state) ->
  gen_statem:start({local, Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
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

get(Statem_name) ->
  gen_statem:call(Statem_name, get).

add(Statem_name, Worker_id) ->
  gen_statem:add(Statem_name, {add, Worker_id}).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
  void.

%% @private
code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

%% @private
init(Worker_ids) ->
  %% Set the initial state to be the list of available Worker_ids
  %% and types.
  {ok, Worker_ids, nil}.

%% @private
callback_mode() ->
  handle_event_function.

%%% state callback(s)

%%
%% Used to select which registered worker is to be used next in
%% a round robin fashion.
%% @private
handle_event({call, From}, get, [H | T], Data) ->
  {next_state, T ++ [H], Data, [{reply, From, H}]};
handle_event({call, From}, {add, Worker_id}, Worker_ids, Data) ->
  {next_state, [Worker_id] ++ Worker_ids, Data, [{reply, From, ok}]};
handle_event({call, From}, get_state, State, Data) ->
  %% Reply with the current state
  {next_state, State, Data, [{reply, From, State}]};
handle_event({call, From}, _, State, Data) ->
  %% Ignore all other events
  {next_state, State, Data, [{reply, From, ignored_event}]}.

%% This code is included in the compiled code only if
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).

%%
%% Unit tests go here.
%%
-endif.
