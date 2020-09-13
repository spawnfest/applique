%%%-------------------------------------------------------------------
%%% @author Chaitanya Chalasani
%%% @copyright (C) 2020, ArkNode.IO
%%% @doc
%%%
%%% @end
%%% Created : 2020-09-13 07:46:41.669779
%%%-------------------------------------------------------------------
-module(evaluate).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,eval/1
        ,connect/0
        ,connect/1
        ,publish/1
        ,publish/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

eval(Code) ->
  do_eval(Code).

connect() ->
  connect(self()).

connect(Pid) ->
  gen_server:cast(?MODULE, {connect, Pid}).

publish(Code) ->
  publish(Code, self()).

publish(Code, Pid) ->
  gen_server:cast(?MODULE, {publish, Code, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, []}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({connect, Pid}, State) ->
  {noreply, lists:usort([Pid|State])};
handle_cast({publish, Code, Pid}, State) ->
  NewState = do_publish(Code, State -- [Pid]),
  {noreply, [Pid|NewState]};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_eval(Code) ->
  case erl_scan:string(Code) of
    {ok, Ts, _} ->
      case erl_parse:parse_exprs(Ts) of
        {ok, Exprs} ->
          case catch erl_eval:exprs(Exprs, []) of
            {value, Fun, _} -> {ok, Fun};
            Other -> Other
          end;
        Error ->
          Error
      end;
    {error, Error, Location} ->
      {error, {Error, Location}}
  end.

do_publish(Code, Pids) ->
  lists:filter(
    fun(Pid) ->
        case is_process_alive(Pid) of
          true ->
            Pid ! Code,
            true;
          false ->
            false
        end
    end,
    Pids
   ).
