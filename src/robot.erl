%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This state machine tracks the state of an industrial robot
%%% that is programmed to pick up computers from an invariat
%%% source location and place the computer in a box that is 
%%% also in an invariant location. The robot knows nothing
%%% about where the computers come from nor where they go.
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(robot).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/2,stop/1]).


%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).
-export([Off/1, On/1]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts a robot process. Since it doesn't link to the process 
%% that started it, it can not be used by a supervisor to start a
%% robot.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Starts a robot process and links it to its calling process. This is
%% the API function a supervisor would use.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the robot.
%%
%% The parameter of stop is an atom that
%% is a registered name of a robot.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    %%{ok,ready,Worker_ids}.
    {ok,{off,center,open,down},[]}.
%%
%% This robot will use the handle_event_function pattern.
%% @private
callback_mode() -> handle_event_function.

%%% state changing API Functions

%%% state pairs:
%%% On - Off
%%% Parked
%%% Shift_State
%%% Brake_On - Brake_Off
%%% Gas on - Gas Off
%%% Wheel_State

Stationary_Off(Car) ->
    gen_statem:call(Car,{Parked, Off}).

Stationary_On(Car) ->
    gen_statem:call(Car,{Brake_On, Parked, On}).

Shift(Car) ->
    gen_statem:call(Car,{Shift_State, On, Gas_Off, Brake_On, Wheel_State}).

Driving(Car) ->
    gen_statem:call(Car,{Shift_State, On, Gas_On, Brake_Off, Wheel_State}).

Braking(Car) ->
    gen_statem:call(Car,{Shift_State, On, Gas_Off, Brake_On, Wheel_State}).







%%
%% Used to put the robot state machine in its next state.
%% @private
handle_event({call,From}, stationary_on, stationary_off,{Statem_name,State_data}) ->
    %Turn the car on
    io:format("Car turned on."),
    next_state = shift,
    {next_state, stationary_on,{stationary_on,State_data},[{reply,From,is_down}]};

handle_event({call,From}, stationary_off, stationary_on,{Statem_name,State_data}) ->
    %Turn the car off
    io:format("Car turned off."),
    {next_state, stationary_off,{stationary_off,State_data},[{reply,From,is_down}]};

handle_event({call,From}, shift, stationary_on,{Statem_name,State_data}) ->
    %Shift the car after turning on
    io:format("Car shifted from stationary."),
    {next_state, shift,{shift,State_data},[{reply,From,is_down}]};

handle_event({call,From}, stationary_on, shift,{Statem_name,State_data}) ->
    %Park the car
    io:format("Car parked."),
    {next_state, stationary_on,{stationary_on,State_data},[{reply,From,is_down}]};

handle_event({call,From}, driving, shift,{Statem_name,State_data}) ->
    %Start driving
    io:format("Started driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, shift, driving,{Statem_name,State_data}) ->
    %Shift the car after driving
    io:format("Car shifted from driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, braking, driving,{Statem_name,State_data}) ->
    %Press the brakes
    io:format("Brakes pressed."),
    {next_state, braking,{braking,State_data},[{reply,From,is_down}]};

handle_event({call,From}, driving, braking,{Statem_name,State_data}) ->
    %Press the gas after braking
    io:format("Car shifted from driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, shift, braking,{Statem_name,State_data}) ->
    %Shift the car after braking
    io:format("Car shifted from braking."),
    {next_state, shift,{shift,State_data},[{reply,From,is_down}]};


%
% a bunch of other state-to-state changes go here.
%
handle_event({call,From},Attemped_state,Current_state,{Statem_name,State_data}) ->
    io:format("Current state, ~p, does not allow a change to ~p state~n",[Current_state,Attemped_state]),
    {next_state,Current_state,{Current_state,State_data},[{reply,From,fail}]}.


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%% Driving from one parking space to another then back to the original spot
%% State Order: Off -> On -> Shift -> Driving -> Braking -> Shift -> Off
handle_call_test_()-> 
    [?_assertEqual({},robot:handle_event({call, stationary_on}, stationary_off, stationary_On)),
        
    ].



%%
-endif.
