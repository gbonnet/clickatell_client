%%%-------------------------------------------------------------------
%%% File    : clickatellworker.erl
%%% Author  : Guillaume Bonnet
%%% @doc 
%%% Contains SMS related functions for the clickatell app
%%% This is the "low-level" class handling the final data formating and sending
%%% @end
%%%
%%% Created : 12 Aug 2012 by guillaume <guillaumebonn@gmail.com>
%%% History : 09 Jan 2013 by Guillaume : cleaned comments for eDoc
%%%-------------------------------------------------------------------
-module(clickatellworker).

-vsn(0.2).

-export([send/2,send/3]).

-include("../include/include.hrl").

%% @doc Sends SMS with the default Sender Id
%% the format of the destination is verified
%% the function puts together all the parameters for the http query on the
%% clickatell API, that includes concatenation.
-spec(send([],[]) -> string()).
send(To,Message)->
    log4erl:log(info,"Send SMS called to ~p, msg: ~p",[To, Message]),

    % Makes sure the To field is in the right format :
    % only digits, full international format without 00 or + in front
    VerifiedDest = verify_number(To),
	    
    case VerifiedDest of
	{error, Reason} ->
	    log4erl:log(error," ~p",[Reason]),
	    {error, Reason};
	_ -> 
	    % Generate URL with the custom sections
	    FinalUrl = get_url(VerifiedDest,Message),
	    log4erl:log(debug,"final url ~p",[FinalUrl]),
	    
	    % Send request and log response
	    HttpResponse = send_request(FinalUrl),
	    log4erl:log(debug,"Http Response : ~p",[HttpResponse]),
	    Status = check_response_status(HttpResponse,To),
	    Status
	    end.

%% @doc same as send/2 but with a Sender Id
%% we verify the sender id's format, if we don't like it we revert back to
%% the default value ?DEFAULT_SENDER_ID
send(To,From,Message) ->
 log4erl:log(info,"Send SMS called to ~p, from ~p, msg: ~p",[To, From, Message]),

    % Makes sure the To field is in the right format :
    % only digits, full international format without 00 or + in front
    VerifiedDest = verify_number(To),
	    
    % Do the same check for the Sender Id
    VerifiedSenderId = verify_number(From),

    case VerifiedDest of
	{error, Reason} ->
	    log4erl:log(error," ~p",[Reason]),
	    {error,Reason};
	_ -> 
	    FinalUrl = case VerifiedSenderId of
			   {error, _} ->
			       % Something was wrong with the sender id's format
			       % revert back to the default sender id
			       get_url(VerifiedDest,Message,?DEFAULT_SENDER_ID);
			   _ -> 
			       % Generate URL with the custom sections
			       get_url(VerifiedDest,Message,VerifiedSenderId)
		       end,
	    log4erl:log(debug,"final url ~p",[FinalUrl]),
	    
	    % Send request and log response
	    HttpResponse = send_request(FinalUrl),
	    log4erl:log(debug,"Http Response : ~p",[HttpResponse]),
	    Status = check_response_status(HttpResponse,To),
	    Status
    end.

%% @doc The actual sending!
%% Sends the Http request that was generated and returns the response
send_request(Url)->
    %http:request(Url).  %% deprecated, won't work from Erlang R15
    httpc:request(Url).  %% Use httpc module instead

%% @doc same as get_url/3 but sets the caller id to the default one
get_url(To,Message) ->
    % No Sender Id specified : use default one
    get_url(To,Message,?DEFAULT_SENDER_ID).

%% @doc puts the elements together and handles the concatenation flags
get_url(To,Message,From) ->
    % Check the message length and if necessary set the concatenation parameter
    Concatenation = get_concatenation(Message),

    % Get the Url paramaters for the sender id
    SenderId = get_sender_id(From),

    % If the message is too long we need to truncate him
    ValidMessage = case string:len(Message) > ?SMS_MAX_LENGTH_CONCAT of
	true ->
	    truncate_message(Message);
	false ->
	    Message
    end,

    % Will encode the string for URL compatibility
    EncodedMessage = edoc_lib:escape_uri(ValidMessage),

    % put the elements together and return
    sms_utils:concat_elements("",[?CLICKATELL_URL,
			"user=" ++ ?CLICKATELL_USERNAME,
			"&password=" ++ ?CLICKATELL_PASSWORD,
			"&api_id=" ++ ?CLICKATELL_API_ID,
			"&to=" ++ To,
			SenderId,
			Concatenation,
			"&text=" ++ EncodedMessage]).


%% @doc checks that the string contains only digits, strips it of 
%% leading 00 or + , also adjust for Irish mobiles entered in national format
verify_number(To) ->
    case string:to_integer(To) of
	{Stripped,[]} ->
	    %% The field contains only digits we carry on
	    %% Stripped won't have any leading 0, nor leading +
	    case string:substr(To,1,string:len(?DEFAULT_LOCAL_FORMAT)) of
		?DEFAULT_LOCAL_FORMAT ->
		    %% changes a local number into its international format
		    string:concat(?DEFAULT_COUNTRY_CODE,integer_to_list(Stripped));
		_ ->
		    % Already in valid format, return a string
		    integer_to_list(Stripped)
	    end;
	_ ->
	    {error,"To/From field must not be null and contain digits only"}
    end.


%% @doc Checks the message length and returns the concat parameter if needs be, 
%% and empty string otherwise
get_concatenation(Message)->
    %% 1st we need to get the Message length : 
    %% some special characters count for 2!
    MessageLength = get_message_length(Message,0),
    
    Count = get_message_count(MessageLength),

    %% if the Count is greater to one we need to send back the concat 
    %% paramater to clickatell
    case Count > 1 of
	true ->
	    string:concat("&concat=",integer_to_list(Count));
        false ->
	   [] %% return an empty string
    end.


%% @doc checks the lengths of the message to return the amount of SMS 
%% this represents, 3 Max
get_message_count(MessageLength) when 
      MessageLength =< ?SMS_MAX_LENGTH -> 1;
get_message_count(MessageLength) when 
      MessageLength > ?SMS_MAX_LENGTH, MessageLength =< (2*?SMS_MAX_LENGTH)-14 -> 2;
get_message_count(MessageLength) when 
      MessageLength > (2*?SMS_MAX_LENGTH)-14, MessageLength =< (3*?SMS_MAX_LENGTH)-21 -> 3;
get_message_count(MessageLength) when 
      MessageLength  > (3*?SMS_MAX_LENGTH)-21 ->
    %% message length is greater than the max authorised length, 
    %% it will be truncated
    3.    


%% @doc Check for special characters that counts for 2 chars 
%% (see Clickatell doc) and returns the total length
get_message_length([],Count) -> Count;
get_message_length([Head|Tail],Count) ->
    case is_special_char(Head) of
	true -> get_message_length(Tail,Count+2);
	false -> get_message_length(Tail,Count+1)
    end.


%% @doc Checks if a character is encoded over 2 characater for the SMS:
%%  \, [, ], {, }, ^, €, |, ~ and line breaking
is_special_char(Char) ->
    case Char of 
	$\ -> true;
	$[ -> true;
	$] -> true;
	${ -> true;
	$} -> true;
	$^ -> true;
	$| -> true;
	$~ -> true;
	8364 -> true; %% euro sign
	13 -> true; %% carriage return
	_ -> false  %% anything else counts for 1
    end.


%% @doc Truncates the message to make sure it is not longer than 
%% the max length authosised. This is only called when the message 
%% is too long.
truncate_message(Message)->
    string:subbstr(Message,0,?SMS_MAX_LENGTH_CONCAT).


%% @doc Generates the Sender Id parameters for the request URL
%% See clickatell documentation for more info
get_sender_id(From) ->
    case From of 
	?DEFAULT_SENDER_ID -> 
	    string:concat("&from=",?DEFAULT_SENDER_ID);
	_ -> 
	    %% req_feat=48 is to ensure Sender ID is set properly 
	    %% (SMS may not use least cost route)
	    string:concat("&req_feat=48&from=",From)
    end.

%% @doc Parses the response from clickatell and return either
%% {ok, ID ...} in case of success or 
%% {error, Reason} in case of failure
check_response_status(Response, Destination)->
    case Response of 
	%% Matchs the HTTP response structure
	{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} ->
	    %% If clickatell response's body looks like "ID: ...." it's a success
	    %% we return the ID since it can be useful to track a SMS for debug
	    case re:run(Body,"^ID:") of
		nomatch ->
		    {error,"Destination : " ++  Destination ++ " " ++  Body};
		{match,_Captured} ->
		    {ok,"Destination : " ++  Destination ++ " " ++  Body}
	    end;
	_ ->
	    {error,unknown}
	end.
