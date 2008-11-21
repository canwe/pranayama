%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% The only reason behind this module is for use of unit
%%% testing: if we are in test mode, we bypass the standard
%%% network related functions in favour of several mocks; with
%%% these mocks we have the ability to fake the response.
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
-module(pranayama_net).

-export([accept_connection/1,
	 call_global_process/2,
	 get_client_info/1,
	 receive_data/2,
	 receive_raw_data/3,
	 send_data/2,
	 set_mock_response/2]).

-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-define(TEST_MODE,true).
-else.
-define(TEST_MODE,false).
-endif.

%%--------------------------------------------------------------------
%% @spec accept_connection(ListeningSocket::socket())-> any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
accept_connection(ListeningSocket)->
    accept_connection(?TEST_MODE,ListeningSocket).

%%--------------------------------------------------------------------
%% @spec accept_connection(_InTestMode::boolean(),ListeningSocket::socket())-> any()
%% @doc
%%  When not in test mode, we call gen_tcp:accept
%% @end
%%--------------------------------------------------------------------
accept_connection(false=_InTestMode,ListeningSocket)->
    gen_tcp:accept(ListeningSocket);
accept_connection(true=_InTestMode,_) ->
    get_mock_response(accept_connection).

%%--------------------------------------------------------------------
%% @spec call_global_process(GlobalProcessName::atom(),
%%                          Message::term())-> any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
call_global_process(GlobalProcessName,Message)
  when is_atom(GlobalProcessName)->
    call_global_process(?TEST_MODE,GlobalProcessName,Message).

%%--------------------------------------------------------------------
%% @spec call_global_process(_InTestMode::boolean(),GlobalProcessName::atom(),
%%                          Message::term())-> any()
%% @doc
%%  When not in test mode, we call gen_server:call
%% @end
%%--------------------------------------------------------------------
call_global_process(false=_InTestMode,GlobalProcessName,Message)->
    gen_server:call({global,GlobalProcessName},Message);
call_global_process(true=_InTestMode,_,_) ->
    get_mock_response(call_global_process).
						   

%%--------------------------------------------------------------------
%% @spec get_client_info(ClientSocket::socket())-> any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_client_info(ClientSocket)->
    get_client_info(?TEST_MODE,ClientSocket).

%%--------------------------------------------------------------------
%% @spec get_client_info(_InTestMode::boolean(),ClientSocket::socket())-> any()
%% @doc
%%  When not in test mode, we call inet:peername().
%% @end
%%--------------------------------------------------------------------
get_client_info(false=_InTestMode,ClientSocket)->
    inet:peername(ClientSocket);
get_client_info(true=_InTestMode,_) ->
    get_mock_response(get_client_info).

%%--------------------------------------------------------------------
%% @spec receive_data(ClientSocket::socket(),
%%                   NumBytesToReceive::integer())-> any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
receive_data(ClientSocket,NumBytesToReceive)->
    receive_data(?TEST_MODE,ClientSocket,NumBytesToReceive).

%%--------------------------------------------------------------------
%% @spec receive_data(_InTestMode::boolean(),ClientSocket::socket(),
%%                   NumBytesToReceive::integer())-> any()
%% @doc
%%  When not in test mode, we call gen_tcp:recv()
%% @end
%%--------------------------------------------------------------------
receive_data(false=_InTestMode,ClientSocket,NumBytesToReceive)->
    gen_tcp:recv(ClientSocket,NumBytesToReceive);
receive_data(true=_InTestMode,_,_) ->
    get_mock_response(receive_data).

%%--------------------------------------------------------------------
%% @spec receive_raw_data(ClientSocket::socket(),
%%                   ContentLength::integer(),
%%                   Timeout::timeout())-> any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
receive_raw_data(ClientSocket,ContentLength,Timeout)->
    receive_raw_data(?TEST_MODE,ClientSocket,ContentLength,Timeout).

%%--------------------------------------------------------------------
%% @spec receive_raw_data(_InTestMode::boolean(),ClientSocket::socket(),
%%                   ContentLength::integer(),
%%                   Timeout::timeout())-> any()
%% @doc
%%  When not in test mode, we call gen_tcp:recv() after setting
%%  our Socket to raw mode.
%% @end
%%--------------------------------------------------------------------
receive_raw_data(false=_InTestMode,ClientSocket,ContentLength,Timeout)->
    inet:setopts(ClientSocket,[{packet,raw}]),
    gen_tcp:recv(ClientSocket,ContentLength,Timeout);
receive_raw_data(true=_InTestMode,_,_,_) ->
    get_mock_response(receive_raw_data).

%%--------------------------------------------------------------------
%% @spec send_data(ClientSocket::socket(),Data::any())->any()
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
send_data(ClientSocket,Data)->
    send_data(?TEST_MODE,ClientSocket,Data).

%%--------------------------------------------------------------------
%% @spec send_data(_InTestMode::boolean(),ClientSocket::socket(),Data::any())->any()
%% @doc
%%  When not in test mode, we call gen_tcp:send()
%% @end
%%--------------------------------------------------------------------
send_data(false=_InTestMode,ClientSocket,Data)->
    gen_tcp:send(ClientSocket,Data);
send_data(true=_InTestMode,_,_) ->
    get_mock_response(send_data).

%%--------------------------------------------------------------------
%% @spec set_mock_response(Action::atom(),Response::term())-> term()
%% @doc
%%  We use the process dictionary.
%% @end
%%--------------------------------------------------------------------
set_mock_response(Action,Response)
  when is_atom(Action)->
    put(Action,Response).



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec get_mock_response(Action::atom())-> term()
%% @doc
%%  We use the process dictionary. A 'stream' allows us
%%  to send back our mock Response, one element at a time.
%% @end
%%--------------------------------------------------------------------
get_mock_response(Action) when is_atom(Action)->
    case get(Action) of

	{stream,Responses} when is_list(Responses)->
	    [NextResponse|RestOfResponses] = Responses,
	     put(Action,{stream,RestOfResponses}),
	     NextResponse;
	Response ->
	    Response
							  
    end.
