# Clickatell_Client #
=====================

Simple Erlang application, with a REST API and a RabbitMQ listener, for sending SMS using Clickatell HTTP API (see www.clickatell.com).

You need to be a registered Clickatell customer and to have enabled their HTTP API for your account to use this app. Note that Clickatell enforces ccontrol over the sender ids to avoid spoofing. All sender ids need to be registered with them.

This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.


## Dependencies ##

**Log4erl** see : https://github.com/ahmednawras/log4erl

Log4erl has used in this app for logging. With the default configuration of the clickatell_client app file, log4erl must be started for it to start. If you don't want to use this logging tool, you can comment out the lines with the reference "log4erl:", and remove the dependency from the app file.

**inets** this is the Erlang app handling HTTP connections. It comes by default with Erlang.


## Usage ##

This app is a full OTP application and can be used as such directly in Erlang. I've also included a simple REST interface to use the app via HTTP GET requests, using Yaws (See: http://yaws.hyber.org/) as a web server; and a RabbitMQ listener.

### Direct ###

From an Erlang shell (assuming the app is installed in your Erlang's lib directory):

    1> application:start(log4erl).
    ok
    2> application:start(inets).
    ok
    3> application:start(clickatell_client).
    ok
    4> clickatell_client:send(["0123456789"],"Hello World!").
    {ok,"Destination : 0123456789 ID: 9a502575840b87e89e33e1d21f46f5a0"}


You'll find the methods exposed in clickatell_client (the Application file) and sms_server.erl :

    -export([send/2,send/3]).

Send/2 is the same as send/3 with the From field set to the default value (as per the value defined in the include.hrl).

The Msg field is a string, that's the SMS content.

The To field, can be :

 - an atom, in which case, it must be one of the contact list label, defined in the include.hrl. For example :
    -define(CONTACTS_LISTS,[{noc,["012345678","001987654321"]},{sales,["011221122","055665566"]}]).
This is a list called 'noc'. Calling clickatell_client:send(noc,"some message"). will send "some message" to all the numbers in that list.

 - a List of numbers for example ["0123456799"] or ["0112344566","00152569696"]
Note that even if there is only 1 destination number, it remains a list of strings!


### Yaws ###

See the related code in /path/to/app/clickatell_client/priv/docroot/index.yaws

    http://localhost:8080/clickatell_client/?action=send_to_many&to=0831234567,0039345678999&msg=Hello%20world%20!
    http://localhost:8080/clickatell_client/?action=send_to_list&to=noc&msg=Hello%20Noc%20!
    http://localhost:8080/clickatell_client/?action=send&to=0039345678999&msg=Hello%20tester%20!

I use clickatell_client app as a Yapp for Yaws web server (also written in Erlang). The main idea is to benefit from Erlang native advantages (fault tolerance, concurrency, light weight etc ...) in a web environment and to get a RESTful interface for the application, which will ease allow me to use it with non-erlang applications easily. (See: http://yaws.hyber.org/yapp_intro.yaws)


The parameters expected are "action", "to", "from" (optional) and "msg".
- "action" atom, send | send_to_many | send_to_list; if not specified, send is assumed.
- "to" string, for send it must be a single phone number, for send_to_many, it must be a succession of phone numbers separated by commas, for send_to_list it must be an atom with the list name, as defined in the include.hrl file.
- "from" is optional, string, it's a valid sender id.
- "msg" is the text for the SMS, it must not contains any & sign.

The order of the parameters is important. (This is probably something I should fix at some stage).


### RabbitMQ ###

sms_queue.erl is set to listen to a RabbitMQ queue. Check the ./include/queue_config.hrl for Queue-specific config elements (queue name, node name ...).

Messages from the AMQP server arrives on this listener direclty (Pid ! msg), so on the OTP laytout, it's handle_info that is the most important function there. The sms_queue module, established the connections to the AMQP broker, and listens to the channel; when it receives a message, it extracts the payload and call sms_server:send/2 or send/3.

The aim of this module is to be able to send SMS from any RabbitMQ client (regardless of its language) running in a reachable Erlang node.


## Build ##

Download the zip file from GIT and unzip the main folder in your Erlang lib folder with the other applications or from your Erlang lib folder run :

    git clone git://github.com/gbonnet/clickatell_client

Then simply run :
    cd clickatell_client
    make

The application is now compiled, you can proceed to the configuration.


## Configuration ##

In /path/to/app/clickatell_client/include/include.hrl you'll find the various global settings you may want to change.

### Country specific settings ###

I've design this app for use in the Republic of Ireland where the call prefix is "353" and mobile numbers start by "08". Clickatell requires phone numbers to be presented in full international format bar the leading +, 00 or 011.

    -define(DEFAULT_COUNTRY_CODE,"353").    % 353 for Ireland, use 44 in the UK, 33 in France ...
    -define(DEFAULT_LOCAL_FORMAT,"08").     % Irish mobiles starts with 08
                                            % In France use "06", in Italy "3", Uk, "07" ...

### Clickatell specific settings ###

In there you'll need to enter your clickatell credentials, API ID and your default sender id (usually your company name).

    -define(CLICKATELL_USERNAME,"myusername").
    -define(CLICKATELL_PASSWORD,"mypassword").
    -define(CLICKATELL_API_ID,"123456").
    -define(DEFAULT_SENDER_ID,"mysenderid").


### Other settings ###

Log4erl creates and appends rotating log files, specify the path where the log4erl.conf file is stored
Typically this is in the priv folder of the OTP app

    -define(DEFAULT_LOG_PATH,"/path/to/log/config").   % no tailing /

You can set lists of contacts to use with the send_to_list (arity 2 or 3) function.
The format of a list is {atom_list_name,["number 1","number 2"...]}
    
    -define(CONTACTS_LISTS,[{noc,["012345678","001987654321"]},{sales,["011221122","055665566"]}]).


## Use the app accross multiple node (distributed) ##

In ./config/ I prepared some config files for "mainnode" and "backupnode". These config uses 2 nodes on the local computer, mainnode@erlangdomain and backupnode@erlangdomain. erlangdomain is set to 127.0.0.1 in my /etc/hosts. You can edit these files for use on your own platform, on a single or multiple servers.

You'll need to register a global name for the "sms_server" so that it can be called from other app on multiple nodes, regardless of where the app is currently running (mainnode or backupnode).


## Comments ##
 - I do not work for Clickatell.
 - I'm interested in feedback on this app, since that's pretty much my 1st application written in Erlang.


