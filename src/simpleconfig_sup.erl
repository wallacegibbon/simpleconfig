-module(simpleconfig_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_specs(CommonTab) ->
    [{simpleconfig, {simpleconfig, start_link, [CommonTab]}, permanent,
      10000, worker, [simpleconfig]}].

init(CommonTab) ->
    SupFlags = #{strategy => one_for_all, intensity => 3, period => 3},
    {ok, {SupFlags, child_specs(CommonTab)}}.

start_link() ->
    CommonTab = ets:new(simpleconfig, [set, public]),
    true = ets:insert(CommonTab, {top_state, #{}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, CommonTab).

