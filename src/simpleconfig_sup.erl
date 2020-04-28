-module(simpleconfig_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_specs() ->
    [#{id => simpleconfig, start => {simpleconfig, start_link, []},
       restart => permanent, shutdown => 10000, type => worker,
       modules => [simpleconfig]}].

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 3, period => 3},
    {ok, {SupFlags, child_specs()}}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

