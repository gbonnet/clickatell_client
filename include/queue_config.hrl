%%%-------------------------------------------------------------------
%%% File    : queue_config.hrl
%%% Author  : Guillaume Bonnet
%%% Description : Contains config elements for the queue of this app
%%%
%%% Created : 20 Nov 2012 by guillaume <guillaumebonn@gmail.com>
%%%-------------------------------------------------------------------

%% Export Env variable in shell
%% ERL_LIBS=/usr/lib/erlang/lib/rabbitmq-erlang-client/dist/ erl -pa ebin

%% Rabbit MQ server
-define(QUEUE_NODE,'rabbit@mydomain.local').

%% Listen queue name
-define(QUEUE_NAME,"sms_queue").

