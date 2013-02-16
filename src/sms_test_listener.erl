%%%-------------------------------------------------------------------
%%% @author Guillaume Bonnet <guillaumebonn@gmail.com>
%%% @doc
%%% 
%%% Test module for the queue listener.
%%% 
%%% @end
%%% Created : 17 Dec 2012   Guillaume
%%% History : 
%%%-------------------------------------------------------------------

-module(sms_test_listener).

-include_lib("eunit/include/eunit.hrl").

%% Dummy Event message for testing the SMS Q : Valid format
-define(DUMMY_SMS_VALID,{{'basic.deliver',
                            <<"amq.ctag-A7ytaudlz1EB52Crll88H_">>,3,false,
                            <<>>,<<"sms_queue">>},
                        {amqp_msg,
                            {'P_basic',undefined,undefined,undefined,2,
                                undefined,undefined,undefined,undefined,
                                undefined,undefined,undefined,undefined,
                                undefined,undefined},
                            <<131,104,2,100,0,4,115,101,110,100,108,0,0,0,3,
                              104,2,100,0,2,116,111,107,0,14,48,48,51,57,51,
                              52,54,53,50,48,48,55,48,51,104,2,100,0,4,102,
                              114,111,109,107,0,14,48,48,51,57,51,52,54,53,
                              50,48,48,55,48,51,104,2,100,0,3,109,115,103,
                              107,0,28,69,85,110,105,116,32,103,101,110,101,
                              114,97,116,101,100,32,116,101,115,116,32,109,
                              101,115,115,97,103,101,106>>}}).

% Dummy Event message for testing the SMS Q : Invalid format
-define(DUMMY_SMS_INVALID,{{'basic.deliver',<<"amq.ctag-gLp4So8bERK7N2XJ8AQjo-">>,30,false,
                            <<>>,<<"email_queue">>},
           {amqp_msg,{'P_basic',undefined,undefined,undefined,2,undefined,
                                undefined,undefined,undefined,undefined,
                                undefined,undefined,undefined,undefined,
                                undefined},
                     <<131,104,7,100,0,5,101,109,97,105,108,107,0,21,103,117,
                       105,108,108,97,117,109,101,64,98,108,117,101,102,97,
                       99,101,46,105,101,100,0,9,117,110,100,101,102,105,110,
                       101,100,100,0,9,117,110,100,101,102,105,110,101,100,
                       100,0,9,117,110,100,101,102,105,110,101,100,107,0,17,
                       104,101,108,108,111,32,119,111,114,108,100,32,101,109,
                       97,105,108,107,0,36,115,105,109,112,108,101,32,101,
                       109,97,105,108,32,119,105,116,104,32,97,108,109,111,
                       115,116,32,110,111,116,104,105,110,103,32,105,110,46>>}}).

%%%===================================================================
%%% Basic tests : server starts / stop
%%%===================================================================

%% @doc Starts, check the up state and stops the SMS Q listener
sms_q_startup_test()->
    % Starts the SMS Q listener
    ?assertMatch({ok, _ServerPid},
		 sms_queue:start_link()),
    % verify that it's already started
    ?assertMatch({error,{already_started, _ErrServerPid}},
		 sms_queue:start_link()),
    % Stops it
    ?assertEqual(ok,sms_queue:stop()).



%%%===================================================================
%%% Functional tests : listen to channel, receive message
%%%===================================================================

%% @doc Tests Event Listener API with a specific test_message function
%% since RabbitMQ sends the message directly. We assert against failures as
%% well as success.
%% This test makes use of a hardcoded Macro: ?DUMMY_SMS_VALID and ?DUMMY_SMS_INVALID
sms_test_() ->
    {setup,fun() -> sms_queue:start_link()  end,
    fun(_) -> sms_queue:stop() end,
     [?_assertEqual(ok,sms_queue:purge()),
      ?_assertMatch({error, _Reason},sms_queue:test_message(?DUMMY_SMS_INVALID)),
      ?_assertEqual(ok,sms_queue:test_message(?DUMMY_SMS_VALID))]}.


