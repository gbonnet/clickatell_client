%%%-------------------------------------------------------------------
%%% File    : sms_server.erl
%%% Author  : Guillaume Bonnet
%%% @doc 
%%%  Main server for this clickatell_client app
%%%  It interacts with the clickatellworker module.
%%% @end
%%%
%%% Created : 22 Oct 2012 by guillaume <guillaumebonn@gmail.com>
%%% History : 09 Jan 2013 by Guillaume : Cleaned comments for eDocs and 
%%%           cleaned the API so that only send/2 and send/3 are exposed
%%%-------------------------------------------------------------------
-module(sms_server).

-behaviour(gen_server).

-vsn(0.2).

-include("../include/include.hrl").

%% API : Exposed to user
-export([send/2,send/3]).

%% API : called by other OTP modules
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}). % not in use in this app

%%====================================================================
%% API
%%====================================================================

start_link() ->
    log4erl:log(info,"~p starting",[?MODULE]),
    % Start the gen server, register the name locally !
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc calls send/3 with default sender id as From field
send(To,Msg) ->
    send(To,?DEFAULT_SENDER_ID,Msg).

%% @doc main API (called by the clickatell_client, sms_queue and yaws app
%% module or directly from the shell)
send(To,From,Msg) when is_atom(To) ->
     Destinations = sms_utils:fetch_list(To),
     case Destinations of 
 	{error, Reason} ->  % could not load list of contacts !
 	    {error,Reason};
 	_ ->
 	    sender(Destinations,From,Msg,[])
     end;

%% To is a list
send(To,From,Msg) ->
      sender(To,From,Msg,[]).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit,true),% required for the supervisor to catch crashes
    log4erl:log(info,"~p init",[?MODULE]),
    {ok, #state{}}.

handle_call({sendsms,Args},_From, State) ->
    case Args of
	[To, From, Msg] ->
	    Reply = clickatellworker:send(To,From,Msg);
	[To,Msg] ->
	    Reply = clickatellworker:send(To,Msg)
     end,
    {reply,Reply, State};

handle_call(Request, _From, State) ->
    log4erl:log(warn,"Unhandled call request received : ~p",[Request]),
    {reply, unhandled, State}.


handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    log4erl:log(warn,"Received direct message: ~p",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    log4erl:log(info,"~p stopping : ~p",[?MODULE,Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Private Functions
%%--------------------------------------------------------------------

%% @doc goes through the list of destinations and returns a list with all
%% the return messages
sender([],_From,_Msg,Acc) ->
     Acc;
sender([To|OtherEndPoints],From,Msg,Acc) ->
    sender(OtherEndPoints,From,Msg,[Acc|sendmessage(To,From,Msg)]).

%% @doc Call to fire up a process to send the individual SMS
sendmessage(To,From,Msg)->
    extract_clean_response(
       gen_server:call(?MODULE,{sendsms,[To,From,Msg]})).

%% @doc extracts the useful section of the OTP standard response for 
%% handle_call/3
extract_clean_response(Response) ->
    case Response of
	{reply,Cleaned,_State} ->
	    Cleaned;
	_ ->
	    Response
    end.
