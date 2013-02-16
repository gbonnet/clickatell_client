%%%-------------------------------------------------------------------
%%% @author Guillaume Bonnet <guillaumebonn@gmail.com>
%%% @doc
%%% 
%%% Test module for the 
%%% 
%%% @end
%%% Created : 19 Dec 2012   Guillaume
%%% History : 
%%%-------------------------------------------------------------------


-module(sms_test_worker).

-include_lib("eunit/include/eunit.hrl").

-include("../include/include.hrl").

%% @doc
sms_send_test() ->
    %% Will fail since To fied has a letter inside
    ?assertMatch({error, _Reason},
 		 clickatellworker:send("012A523","Dummy Message from EUnit.")),
    %% Will fail since To field is blank
    ?assertMatch({error, _Reason},
 		 clickatellworker:send("","Dummy Message from EUnit.")),
    %% Same tests with send/3
    ?assertMatch({error, _Reason},
 		 clickatellworker:send("012A523","012345679","Dummy Message from EUnit.")),
    ?assertMatch({error, _Reason},
 		 clickatellworker:send("","0123456789","Dummy Message from EUnit.")),

    %% Send to valid number but with unregistered sender id 
    %% (ie: not store on clickatell's site for our account)
    ?assertMatch({error,_Response},
 		 clickatellworker:send("00393465200703","0123456789","Dummy Message from EUnit.")),


    %% Success
    ?assertMatch({ok,_Response},
 		 clickatellworker:send("00393465200703","00393465200703","Dummy Message from EUnit.")).

 
