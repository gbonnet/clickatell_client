%%%-------------------------------------------------------------------
%%% File    : sms_server.erl
%%% Author  : Guillaume Bonnet
%%% @doc 
%%%  Main server for this clickatell_client app
%%%  It interacts with the clickatellworker module.
%%% @end
%%%
%%% Created : 22 Oct 2012 by guillaume <guillaumebonn@gmail.com>
%%% History : 09 Jan 2013 by Guillaume : Clean comments for eDoc
%%%           17 Jan 2013 by Guillaule : added test_message for EUnit tests
%%%
%%%-------------------------------------------------------------------
-module(sms_queue).

-behaviour(gen_server).

-vsn(0.2).

%% General App include
-include("../include/include.hrl").

%% Queue specific include file
-include("../include/queue_config.hrl").

%% path to the AMQP hrl file with record definitions
%% to run this app in the shell you'll need to start it with :
%% ERL_LIBS=/usr/lib/erlang/lib/rabbitmq-erlang-client/dist/ erl -pa ebin
-include_lib("/usr/lib/erlang/lib/rabbitmq-erlang-client/include/amqp_client.hrl").

%% API : Exposed to user
-export([purge/0,test_message/1]).

%% API : called by other OTP modules and test scripts
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the gen server, register the name locally
start_link() ->
    log4erl:log(info,"~p starting",[?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc stops the gen server
stop() ->
    gen_server:cast({local,?MODULE}, stop).

%% @doc Clears the queue of zombie messages
purge() ->
    gen_server:cast(?MODULE,purge).

%% @doc Function used by the test module
-spec(test_message(tuple()) -> ok | error).
test_message(Message) ->
    gen_server:call(?MODULE,{tester,Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Will hook to the queue defined in config file, open the 
%% connection and call the listen_to_channel function
%% The return value is the state for our server, in the format
%% {Channel, Connection}
init([]) ->
    process_flag(trap_exit,true),% required for the supervisor to catch crashes
    log4erl:log(info,"~p init",[?MODULE]),
    Hook = amqp_connection:start(#amqp_params_direct{node=?QUEUE_NODE}),
    State  =  case Hook of
		  {ok, Connection} ->
		      {ok, Channel} = amqp_connection:open_channel(Connection),
		      listen_to_channel(Channel),
		      {Channel, Connection};
		  {error, Error} ->
		      log4erl:log(error,"Connection failed ~p",[Error]),
		      error
	      end,
    {ok, State}.

%% @doc Used by the test module only
handle_call({tester,Request}, _From, {Channel,Connection}) ->
    log4erl:log(debug,"Test request received : ~p",[Request]),
    Reply = handle_message(Request,Channel),
    {reply, Reply, {Channel,Connection}}.

handle_cast(purge, State) ->
    case State of 
	{Channel, _Connection} ->
	    Reply = amqp_channel:call(Channel, 
				      #'queue.purge'{queue = <<?QUEUE_NAME>>}),
	    case Reply of
		{'queue.purge_ok',MessageCount} ->
		    log4erl:log(info,"Purgining Queue. Message Removed : ~p", 
				[MessageCount]);
		_ ->
		    log4erl:log(error,"Queue purge error: ~p",
				    [Reply])
	    end;
	
	_OtherState ->
	    log4erl:log(error,"Unknown State for purging")
    end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

%% @doc Main function for RabbitMQ listeners
handle_info(Info, State) ->
    log4erl:log(debug,"Received direct message: ~p",[Info]),
    Reply = case State of 
	{Channel, _Connection} ->
	    handle_message(Info,Channel);
	OtherState ->
	    log4erl:log(error,"Unknown state ~p",[OtherState]),
	    error
     end,
    log4erl:log(debug,"reply: ~p~nState ~p",[Reply,State]),
    {noreply,  State}.


%% @doc Function: terminate(Reason, State) -> void()
%% It needs to close the channel and the connection to the AMQP broker
terminate(Reason, {Channel, Connection}) ->
    log4erl:log(info,"~p stopping : ~p",[?MODULE,Reason]),
    close_channel(Channel),
    close_connection(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Private Functions
%%--------------------------------------------------------------------

%% @doc Channel declaration and consumtion
%% this is called by init/1 and returns the Channel listener PID
listen_to_channel(Channel)->
    amqp_channel:call(Channel, #'queue.declare'{queue = <<?QUEUE_NAME>>,
                                                durable = true}),
    log4erl:log(info,"Queue Waiting for messages"),

    % This tells RabbitMQ not to give more than one message to a worker at a time. Or, in other words, don't dispatch a new message to a worker until it has processed and acknowledged the previous one. Instead, it will dispatch it to the next worker that is not still busy.
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<?QUEUE_NAME>>},
                           self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    
    %% Return the channel listener PID
    Channel.

%% @doc called by handle_info to handle the received data : parse, decode,
%%  action and acknoledge
handle_message(Message, Channel) ->
    case Message of
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}} ->

	    %% Decode message from binary, 
	    %% the safe option is to avoid DoS attacks
	    DecodedBody = binary_to_term(Body), % ,[safe]
            log4erl:log(info,"Received ~p", [DecodedBody]),

	    Response = case DecodedBody of 
		{send,[{to,To},{from,From},{msg,Msg}]} ->
		    log4erl:log(info,"Send To ~p, From ~p, Msg ~p",[To,From,Msg]),
		    sms_server:send(To,From,Msg);
		{send,[{to,To},{msg,Msg}]} ->
		    log4erl:log(info,"Send To ~p,  Msg ~p",[To,Msg]),
		    sms_server:send(To,Msg);
		Any ->
		    log4erl:log(warn,"Unhandled message! ~p",[Any]),		    
		    {error, unhandled_message}	       
	    end,

	    log4erl:log(info,"Response: ~p",[Response]),

	    % we send the acknoledgement : the message was dealt with successfully.
	    % if we don't do that the message will get retransmitted by the server
	    % this is useful if a client dies while processing a message
	    log4erl:log(debug,"Sending Ack to ~p",[Channel]),
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

	    %% Sends the reponse back
	    Response;
	Any ->
	    log4erl:log(warn,"Unhandled message received. ~p",[Any])
        end.

%% @doc closes the AMQP Channel
close_channel(Channel) ->
    log4erl:log(info,"Closing Channel : ~p",[Channel]),
    catch amqp_channel:close(Channel),
    ok.

%% @doc closes the AMQP Connection
close_connection(Connection) ->
    log4erl:log(info,"Closing Connection : ~p",[Connection]),
    catch amqp_connection:close(Connection),
    ok.
