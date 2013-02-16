%%%-------------------------------------------------------------------
%%% @author guillaume <guillaume@guillaume-laptop>
%%% @copyright (C) 2012, guillaume
%%% @doc
%%% Clickatell client app file
%%% @end
%%% Created : 22 Oct 2012 by guillaume <guillaume@guillaume-laptop>
%%% History : 09 Jab 2013 by Guillaume : cleaned comments for eDoc
%%%-------------------------------------------------------------------
-module(clickatell_client).

-behaviour(application).

-vsn(0.2).

%% Application Callbacks
-export([start/2, stop/1]).

%% User API
-export([send/2,send/3]).

-include("../include/include.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the client. normal, failover and takeover cases are handled,
%% so the app is already set for working in active/stand-by mode.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(normal, _StartArgs) ->
    % Set logger
    Path = ?DEFAULT_LOG_PATH ++ "/priv/log4erl.conf",
    log4erl:conf(Path),
    log4erl:log(info,"Clickatell Erlang Client starts : normal"),
    % Call top supervisor
    case sms_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    log4erl:log(error,"Could not start normal Sup."),
	    {error, Error}
     end;

%% @doc The main application (or its server/connectivity) failed
%% the (next) backup application starts
start({failover,_OtherNode}, _StartArgs) ->
    % Set logger
    Path = ?DEFAULT_LOG_PATH ++ "/priv/log4erl.conf",
    log4erl:conf(Path),
    log4erl:log(info,"Clickatell Erlang Client starts : failover"),
    % Call top supervisor
    case sms_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    log4erl:log(error,"Could not start failover Sup."),
	    {error, Error}
     end;

%% @doc The main application has recovered, use it again
%% after having gracefully shutdown the back up app
%% Note : the takeover procedure 1st starts the app and 
%% only when it's fully started, it will shutdown the back up application.
%% This would result in a conflict with the globally registered name: sms_server
%% That's why we 1st need to unregister it.
start({takeover,_OtherNode}, _StartArgs) ->
    % Set logger
    Path = ?DEFAULT_LOG_PATH ++ "/priv/log4erl.conf",
    log4erl:conf(Path),
    log4erl:log(info,"Clickatell Erlang Client starts: takeover"),
    log4erl:log(into,"Unregistering sms_gen_server from global registery"),
    global:unregister_name('sms_gen_server'),
    % Call top supervisor
    case sms_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    log4erl:log(error,"Could not start takeover Sup."),
	    {error, Error}
     end.
 
stop(_State) ->
    log4erl:log(info,"Clickatell Erlang Client stops~nBye!"),
    ok.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Main API to send SMS
%% To can be an atom (in which case it must be a label for a registered 
%% list of phone numbers in the include.hrl file; or can directly be a list 
%% of phone numbers. Msg is a string with the message to send. This function
%% uses the default sender id.
send(To,Msg) ->
    sms_server:send(To,Msg).

%% @doc same but with the From field set to be used as Sender Id.
%% Note that for the Sender Id to work with Clickatell you need to 
%% register it on your account. See clickatell website for more info.
send(To,From,Msg) ->
    sms_server:send(To,From,Msg).
