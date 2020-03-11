%%%-------------------------------------------------------------------
%% @doc datalog public API
%% @end
%%%-------------------------------------------------------------------

-module(datalog_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    datalog_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
