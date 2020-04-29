-module(simpleconfig).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, code_change/3]).

-export([start_link/1, stop/0]).

-export([lookup/2, lookup/1, update/1]).

-define(SERVER, ?MODULE).

update(Filename) when is_list(Filename); is_binary(Filename) ->
    gen_server:call(?MODULE, {update, Filename});
update(default) ->
    gen_server:call(?MODULE, {update, default_cfgfile()}).

lookup(Key, Default) ->
    gen_server:call(?MODULE, {lookup, Key, Default}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

start_link(CommonTab) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CommonTab, []).

stop() ->
    gen_server:call(?MODULE, stop).


default_cfgfile() ->
    case os:getenv("DEVELOPMENT") of
	"true" ->
	    config_file(develop);
	_ ->
	    config_file(release)
    end.

config_file(release) ->
    filename:join([code:root_dir(), "config", "params"]);
config_file(develop) ->
    filename:join(["config", "dev.config"]).


handle_call({update, Filename}, _From, {CommonTab, _} = State) ->
    case file:consult(Filename) of
	{ok, RawConfig} ->
	    Configuration = maps:from_list(RawConfig),
	    ets:insert(CommonTab, {top_state, Configuration}),
	    {reply, ok, {CommonTab, Configuration}};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;

handle_call({lookup, Key, Default}, _From, {_, Config} = State) ->
    {reply, maps:get(Key, Config, Default), State};

handle_call({lookup, Key}, _From, {_, Config} = State) ->
    {reply, maps:get(Key, Config), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


init(CommonTab) ->
    case ets:lookup(CommonTab, top_state) of
	[{top_state, Config}] ->
	    {ok, {CommonTab, Config}};
	_ ->
	    {ok, {CommonTab, #{}}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

