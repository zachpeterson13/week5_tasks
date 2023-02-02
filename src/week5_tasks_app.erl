%%%-------------------------------------------------------------------
%% @doc week5_tasks public API
%% @end
%%%-------------------------------------------------------------------

-module(week5_tasks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    week5_tasks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
