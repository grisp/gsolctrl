% @doc gsolctrl top level supervisor.
% @end
-module(gsolctrl_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, {#{strategy => one_for_all,
                   intensity => 10,
                   period => 60}, 
                  [
                   #{id => influxdb,
                     start => {influxdb, start_link, []}},
                   #{id => simple_ctrl,
                     start => {simple_ctrl, start_link, []}
                    }]}}.
