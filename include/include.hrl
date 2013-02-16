%%%-------------------------------------------------------------------
%%% File    : include.hrl
%%% Author  : Guillaume Bonnet
%%% Description : Contains Defines for the Clickatell SMS client
%%%
%%% Created : 23 Aug 2012 by guillaume <guillaumebonn@gmail.com>
%%%-------------------------------------------------------------------

%% Country Code
-define(DEFAULT_COUNTRY_CODE,"353").    %% 353 for Ireland, use 44 in the UK, 33 in France ...
-define(DEFAULT_LOCAL_FORMAT,"08").     %% Irish mobiles starts with 08
                                        %% In France use "06", in Italy "3", Uk, "07" ...

%% Clickatell base HTTP API URL 
-define(CLICKATELL_URL,"http://api.clickatell.com/http/sendmsg?").

%% Customer Specific Credentials and settings
-define(CLICKATELL_USERNAME,"USERNAME").
-define(CLICKATELL_PASSWORD,"PASSWORD").
-define(CLICKATELL_API_ID,"API_ID").
-define(DEFAULT_SENDER_ID,"SENDER_ID").

%% SMS constants
-define(SMS_MAX_LENGTH,159).            %% after that we need to concatenate the SMS.
-define(SMS_MAX_LENGTH_CONCAT,456).     %% maxmimum length authorised (3 sms in one), anything above that is truncated.

%% Log Config Path
-define(DEFAULT_LOG_PATH,"/usr/lib/erlang/lib/clickatell_client").

%% Lists, the format of a list is {atom_list_name,["number 1","number 2"...]}
-define(CONTACTS_LISTS,[{noc,["012345678","0039323456789"]}]).

