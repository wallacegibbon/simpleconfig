-module(simpleconfig).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, code_change/3]).

-export([start_link/0, stop/0]).

-export([lookup/2, lookup/1, update/1]).

-define(SERVER, ?MODULE).


lookup(Key, Default) ->
    gen_server:call(?MODULE, {lookup, Key, Default}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

update(Filename) ->
    gen_server:call(?MODULE, {update, Filename}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).


handle_call({update, Filename}, _From, State) ->
    case file:consult(Filename) of
	{ok, Configuration} ->
	    {reply, ok, maps:from_list(Configuration)};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;

handle_call({lookup, Key, Default}, _From, State) ->
    {reply, maps:get(Key, State, Default), State};

handle_call({lookup, Key}, _From, State) ->
    {reply, maps:get(Key, State), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


init([]) ->
    {ok, #{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

