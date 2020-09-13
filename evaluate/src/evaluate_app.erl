%%%-------------------------------------------------------------------
%% @doc evaluate public API
%% @end
%%%-------------------------------------------------------------------

-module(evaluate_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    evaluate_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
