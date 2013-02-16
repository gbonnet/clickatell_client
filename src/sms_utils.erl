%%%-------------------------------------------------------------------
%%% File    : sms_utils.erl
%%% Author  : guillaume Bonnet
%%% @doc
%%% Useful non-telco functions for the SMS App
%%% @end
%%%
%%% Created : 12 Aug 2012 by guillaume <guillaumebonn@gmail.com>
%%% History : 09 Jan 2013 by Guillaume : Set comments for eDoc
%%%-------------------------------------------------------------------
-module(sms_utils).

-vsn(1.1).

-include("../include/include.hrl").

-export([concat_elements/2,fetch_list/1]).

%% @doc concats the Head of a list to the Accumulator string
%% then returns the accumulator at the end of the recursion
concat_elements(Acc,[]) -> Acc;
concat_elements(Acc,[Head|Tail]) ->
   concat_elements(string:concat(Acc,Head),Tail).

%% @doc Looks for contacts list in the include.hrl file and returns 
%% the phone numbers in a list, ready to be used for sending by the 
%% sms_gen_server:send_to_many (arity 2 or 3) function. 
fetch_list(ListName) ->
    List = lists:keyfind(ListName,1,?CONTACTS_LISTS),
    case List of 
	false ->  % could not load list of contacts !
	    {error,contact_list_not_found};
	{_ListName,To} ->
	    To
    end.
