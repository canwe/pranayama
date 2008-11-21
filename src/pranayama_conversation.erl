%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%    By  'conversation' we mean the act of receiving
%%%    an http request, generating the response (via
%%%    the delegated webapp), then sending back the
%%%    http response to the connection client.
%%%
%%%    As you'll see, we delegate nearly all the work
%%%    to other modules, primarily <em>pranayama_request_reader</em>
%%%    and <em>pranayama_response_writer</em>.
%%%
%%% ==Licence==
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%% @end
%%% Created : 22 Nov 2008
%%%-------------------------------------------------------------------

-module(pranayama_conversation).

-export([start/2]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% @spec start(AppConfig::app_config(),ListeningSocket::socket()) -> none() 
%% @doc
%%  We must first offer a connection; once accepted we read
%%  the request in as an #http_request which we then send off
%%  to <em>pranayama_response_writer</em> to  have our response generated and sent to the client; finally
%%  we notify the event manager of this completed conversation, and
%%  exit.
%%
%%  The essential step of notifying our connection manager 
%%  (pranayama_conversationalist) our connection acceptance
%%  takes place within offer_conversation.
%%
%% @end
%%--------------------------------------------------------------------
start(AppConfig,ListeningSocket)
when is_record(AppConfig,app_config)->

    ClientSocket = offer_connection(ListeningSocket),    
    HttpRequest =  pranayama_request_reader:read(AppConfig,ClientSocket),
    HttpResponse = pranayama_response_writer:write(AppConfig,HttpRequest,ClientSocket),

    pranayama_event_manager:notify({response,HttpResponse}),
    
    exit(completed).



%%--------------------------------------------------------------------
%% @spec offer_connection(ListeningSocket::socket()) -> ClientSocket::socket() 
%% @doc
%%  It is important to note that this is a blocking call which
%%  means that this module must be spawned.
%%
%%  There are 3 scenarios that we expect:
%%  <ol>
%%  <li> A client accepts our offered connection returning a ClientSocket;
%%     immediately after that essential notification is sent back to our manager
%%     (<em>pranayama_conversationalist</em>).</li>
%%  
%%  <li> Our Listening Socket is found to be closed; we have little choice
%%     but to exit this process, to be replaced by another.</li>
%%  
%%  <li> Some other nonsense occurs and again, we exit this process.</li>
%%  </ol>
%% @end
%%--------------------------------------------------------------------
offer_connection(ListeningSocket)->
    
    case pranayama_net:accept_connection(ListeningSocket) of
	
	{ok,ClientSocket}->
	    gen_server:cast(pranayama_conversationalist,{offer_accepted,self()}),
	    ClientSocket;
	{error,closed} ->
	    exit(listening_socket_connection_closed);
	Otherwise ->
	    pranayama_logger:error("Failed to accept connection:~p",[Otherwise]),
	    exit(failed_to_accept_connection)
    end.

  

%%%===================================================================
%%% Test functions
%%%===================================================================

%% @hidden
start_test()->

    AppConfig = #app_config{},
    ListeningSocket = [],
    ClientSocket = [],

    WebAppAlias = "some_app",
    WebAppPath = "somepath",
    URI = WebAppAlias ++ "/" ++ WebAppPath ++ "?fname=Nina&lname=Simone",
    Version = {1,1},
    ClientIP = {192,168,1,1},
    ClientPort = 6464,
    ClientDescription = "JustLookingThanks 0.4",
    
    ReceiveDataResponse = [{ok,{http_request,"GET",{undefined,URI},Version}},
		    {ok, {http_header, undefined, "User-Agent",undefined, ClientDescription }},	   
		    {ok, http_eoh}],

    pranayama_net:set_mock_response(accept_connection,{ok,ClientSocket}),
    pranayama_net:set_mock_response(receive_data,{stream,ReceiveDataResponse}),
    pranayama_net:set_mock_response(get_client_info,{ok,{ClientIP,ClientPort}}),
    pranayama_net:set_mock_response(call_global_process,{stream,[{get_web_app_global_name,"AppName"},
								 {generate_response,#http_response{}}
								]}),
    
    ?assertExit(completed,start(AppConfig,ListeningSocket)),
    
    ok.
		    
%% @hidden
offer_connection_test()->

    MockClientSocket = [],
    pranayama_net:set_mock_response(accept_connection,{ok,MockClientSocket}),
    ?assertMatch(MockClientSocket,offer_connection(MockClientSocket)),
    
    pranayama_net:set_mock_response(accept_connection,{error,closed}),
    ?assertExit(listening_socket_connection_closed,offer_connection(MockClientSocket)),

    pranayama_net:set_mock_response(accept_connection,{error,something_else}),
    ?assertExit(failed_to_accept_connection,offer_connection(MockClientSocket)),
    

    ok.


