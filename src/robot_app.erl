%%%-------------------------------------------------------------------
%% @doc robot public API
%% @end
%%%-------------------------------------------------------------------

-module(robot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    robot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
