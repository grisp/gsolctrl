-module(influxdb).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, record_data/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_data(Type, Sensor, Value) ->
    gen_server:cast(?MODULE, {record_data, Type, Sensor, Value}).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({record_data, temp, Sensor, Value}, State) ->
    record_temp(Sensor, Value),
    {noreply, State};
handle_cast({record_data, act, Act, Value}, State) ->
    record_act(Act, Value),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

record_temp(Sens, Value) ->
    Data = iolist_to_binary(
             io_lib:format("sol,type=temp,unit=celsius,sensor=~p value=~f\n", 
                           [Sens, Value])),
    httpc:request(post, {"http://yrael:8086/write?db=gsolctrl", [], 
                         "application/binary", 
                         Data}, [], []).

record_act(Act, Value) ->
    Data = iolist_to_binary(
             io_lib:format("sol,type=act,actuator=~p value=~b\n", 
                           [Act, map_act(Value)])),
    httpc:request(post, {"http://yrael:8086/write?db=gsolctrl", [], 
                         "application/binary", 
                         Data}, [], []).

map_act(on) ->
    1;
map_act(off) ->
    0;
map_act(loading) ->
    1;
map_act(bypass) ->
    0.


