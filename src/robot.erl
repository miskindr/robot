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
-export([stationary_Off/2, stationary_On/2, shift/2, driving/2, braking/2]).


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
    {ok,{off,center,open,down},[Worker_ids]}.
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

stationary_Off(Car, State) ->
    StateOnOff = {'Off'},
    StateParked = {'Yes'},
    StateBreak = {'Off'},
    StateShift = {'Park'},
    StateGas = {'Off'},
    StateWheel = {'Middle'},
    State = {StateOnOff, StateParked, StateBreak, StateShift, StateGas, StateWheel},
    {Car,State}.

stationary_On(Car, State) ->
    StateOnOff = {'On'},
    StateParked = {'Yes'},
    StateBreak = {'Off'},
    StateShift = {'Park'},
    StateGas = {'Off'},
    StateWheel = {'Middle'},
    State = {StateOnOff, StateParked, StateBreak, StateShift, StateGas, StateWheel},
    {Car, State}.

shift(Car, State) ->
    StateOnOff = {'On'},
    StateParked = {'Yes'},
    StateBreak = {'Off'},
    StateShift = {'Reverse'},
    StateGas = {'On'},
    StateWheel = {'Right'},
    State = {StateOnOff, StateParked, StateBreak, StateShift, StateGas, StateWheel},
    {Car, State}.

driving(Car, State) ->
    StateOnOff = {'On'},
    StateParked = {'Yes'},
    StateBreak = {'Off'},
    StateShift = {'Drive'},
    StateGas = {'On'},
    StateWheel = {'Right'},
    State = {StateOnOff, StateParked, StateBreak, StateShift, StateGas, StateWheel},
    {Car, State}.

braking(Car, State) ->
    StateOnOff = {'On'},
    StateParked = {'Yes'},
    StateBreak = {'On'},
    StateShift = {'Drive'},
    StateGas = {'On'},
    StateWheel = {'Right'},
    State = {StateOnOff, StateParked, StateBreak, StateShift, StateGas, StateWheel},
    {Car, State}.







%%
%% Used to put the robot state machine in its next state.
%% @private
handle_event({call,From}, State) ->
    %Turn the car on
    {next_state, stationary_on,{stationary_on,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Turn the car off
    io:format("Car turned off."),
    {next_state, stationary_off,{stationary_off,State_data},[{reply,From,is_down}]};

handle_event({call,From},  State) ->
    %Shift the car after turning on
    io:format("Car shifted from stationary."),
    {next_state, shift,{shift,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Park the car
    io:format("Car parked."),
    {next_state, stationary_on,{stationary_on,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Start driving
    io:format("Started driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Shift the car after driving
    io:format("Car shifted from driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Press the brakes
    io:format("Brakes pressed."),
    {next_state, braking,{braking,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Press the gas after braking
    io:format("Car shifted from driving."),
    {next_state, driving,{driving,State_data},[{reply,From,is_down}]};

handle_event({call,From}, State) ->
    %Shift the car after braking
    io:format("Car shifted from braking."),
    {next_state, shift,{shift,State_data},[{reply,From,is_down}]};


%
% a bunch of other state-to-state changes go here.
%
handle_event({call,From}, State) ->
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
    [
        % ?_assertEqual({},robot:handle_event({call, stationary_on}, stationary_off, stationary_On)),
        
    ].



%%
-endif.
