-module(simpleconfig_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child_list(CommonTab) ->
    [{simpleconfig, {simpleconfig, start_link, [CommonTab]}, permanent,
      10000, worker, [simpleconfig]}].

init(CommonTab) ->
    {ok, {{one_for_one, 10, 10}, child_list(CommonTab)}}.

start_link() ->
    CommonTab = ets:new(simpleconfig, [set, public]),
    true = ets:insert(CommonTab, {top_state, #{}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, CommonTab).

