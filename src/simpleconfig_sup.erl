-module(simpleconfig_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_specs(CommonTab) ->
    [#{id => simpleconfig, start => {simpleconfig, start_link, [CommonTab]},
       restart => permanent, shutdown => 10000, type => worker,
       modules => [simpleconfig]}].

init(CommonTab) ->
    SupFlags = #{strategy => one_for_all, intensity => 3, period => 3},
    {ok, {SupFlags, child_specs(CommonTab)}}.

start_link() ->
    CommonTab = ets:new(simpleconfig, [set, public]),
    true = ets:insert(CommonTab, {top_state, #{}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, CommonTab).

