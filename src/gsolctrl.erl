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
    grisp_led:flash(1, yellow, 500),
    {ok, Sup}.

stop(_State) -> ok.
