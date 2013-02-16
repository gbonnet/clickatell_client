%%%-------------------------------------------------------------------
%%% @author Guillaume Bonnet
%%% @doc
%%% Top supervisor for the Clickatell_client SMS application.
%%% It starts and controls the SMS worker and the RabbitMQ listener 
%%% @end
%%% Created : 22 Oct 2012 by guillaume <guillaumebonn@gmail.com>
%%% History : 09 Jan 2013 by Guillaume : Cleaned comments and set them for eDoc
%%%-------------------------------------------------------------------
-module(sms_sup).

-behaviour(supervisor).

-vsn(0.2).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    log4erl:log(info,"sms_sup starts"),

    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    %% Starts SMS gen server
    SMSServer = {'SMSServer', {'sms_server', start_link, []},
	      Restart, Shutdown, Type, ['sms_server']},

    %% Starts Rabbit MQ Queue listner
    QueueListner =  {'QueueListner', {'sms_queue', start_link, []},
		     Restart, Shutdown, Type, ['sms_queue']},

    {ok, {SupFlags, [SMSServer,QueueListner]}}.

