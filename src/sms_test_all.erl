%%-------------------------------------------------------------------
%%% @author Guillaume Bonnet <guillaumebonn@gmail.com>
%%% @doc
%%% 
%%% Master module for EUnit tests : it simply calls all the other
%%% test modules, so that the developer simply needs to call :
%%% test_all:test(). from the shell and all the EUnit test will run
%%% 
%%% @end
%%% Created : 17 Dec 2012   Guillaume
%%% History : 
%%%-------------------------------------------------------------------

-module(sms_test_all).

-include_lib("eunit/include/eunit.hrl").

%% @doc it does what it says on the tin. This is a EUnit module, so to call this, just call : test_all:test(). 
all_test_() ->
    [{module, sms_test_listener},       %% Q listener tests
     {module, sms_test_worker}].        %% Clickatell worker (low level logic) tests

