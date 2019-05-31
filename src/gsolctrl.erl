% @doc gsolctrl public API.
% @end
-module(gsolctrl).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> 
    {ok, Sup} = gsolctrl_sup:start_link(),
    Random = fun() -> 
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1} 
    end, 
    grisp_led:pattern(1, [{100, Random}]),
    grisp_led:pattern(2, [{100, Random}]),
    {ok, Sup}.

stop(_State) -> ok.
