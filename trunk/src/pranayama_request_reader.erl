%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% Responsible for reading in the request from the client
%%% Socket, and converting it into a #http_request.
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
-module(pranayama_request_reader).
-compile(export_all).
-export([read/2]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% @spec read(AppConfig::app_config(),ClientSocket::socket())
%%            -> http_request() 
%% @doc
%% 
%% We build up the #http_request in layers: we obtain some meta
%% info such as the method, read the headers, then the body; finally
%% there is some post-processing / enhancements that we do to make
%% the record more convenient down the pipe.
%%
%% @end
%%--------------------------------------------------------------------
read(AppConfig,ClientSocket) 
  when is_record(AppConfig,app_config)->
    
    InitialHttpRequest = start_to_read_request(AppConfig,ClientSocket),

    Headers = read_request_headers(ClientSocket),
    Body = read_request_body(ClientSocket,InitialHttpRequest#http_request.method,Headers,AppConfig),
    
    enhance(InitialHttpRequest#http_request{headers=Headers,body=Body}).


%%--------------------------------------------------------------------
%% @spec start_to_read_request(AppConfig::app_config(),ClientSocket::socket())
%%            -> http_request() 
%% @doc
%%
%%  We grab only the first line; to remind we're using the
%%  {packet,http} approach. Appreciation goes to this generous posting:
%%  [http://wiki.trapexit.org/index.php/A_fast_web_server_demonstrating_some_undocumented_Erlang_features].
%%  Apparently this undocumented feature is not yet official but it
%%  appears to be quite usable. However, there is a possibility of a coming change:
%%  [http://groups.google.com/group/erlang-questions/msg/b34b842fe1a80cbc]; this
%%  would affect us as we are using a passive socket.
%%
%% @end
%%--------------------------------------------------------------------
start_to_read_request(AppConfig,ClientSocket)
  when is_record(AppConfig,app_config) ->

    case pranayama_net:receive_data(ClientSocket,0) of  

	{ok, {http_request, RawMethod, {_,URI} = RawPath, Version}}-> 
	
	    pranayama_logger:trace("Starting to read request: method(~p), path(~p), version(~p)",
			   [RawMethod,RawPath,Version]),

	    {ok,{ClientIP,ClientPort}} = pranayama_net:get_client_info(ClientSocket),
	    
	    #http_request{method=pranayama_utils:figure_request_method(RawMethod),
			  uri=URI,
			  version=Version,
			  client_ip=ClientIP,
			  client_port=ClientPort,
			  client_username=undefined
			 };
	{error,einval} ->
	    throw(failed_to_read_request)
    end.



%%--------------------------------------------------------------------
%% @spec read_request_headers(ClientSocket::socket())
%%            -> headers() 
%% @doc
%% Seed for read_request_headers/2.
%% @end
%%--------------------------------------------------------------------
read_request_headers(ClientSocket)->
    read_request_headers(ClientSocket,[]).
%%--------------------------------------------------------------------
%% @spec read_request_headers(ClientSocket::socket(),headers())
%%            -> headers() 
%% @doc
%% We build up our headers one by one, ignoring any errors and
%% continuing until we receive the eof signal.
%% @end
%% @todo Should we have a timeout?
%%--------------------------------------------------------------------
read_request_headers(ClientSocket,Headers)
  when is_list(Headers)->
      
    case pranayama_net:receive_data(ClientSocket,0) of
    
	{ok, {http_header, _, Key, _, Value}} when is_atom(Key) -> 
	  Header = {atom_to_list(Key),Value},
	  read_request_headers(ClientSocket,[Header|Headers]);

	{ok, {http_header, _, Key, _, Value}} -> 
	  Header = {Key,Value},
	  read_request_headers(ClientSocket,[Header|Headers]);
	
	{error, {http_error, Error}} ->
	  pranayama_logger:warn("Conversation received an http error while trying to read headers. ~p",[Error]),
	  read_request_headers(ClientSocket,Headers);  

	{ok, http_eoh} ->
	  lists:reverse(Headers);

	_UnexpectedLine->
	    pranayama_logger:error("Unexpected line while parsing header:~p",[_UnexpectedLine]),
	    read_request_headers(ClientSocket,Headers)
		  	  
    end.

%%--------------------------------------------------------------------
%% @spec read_request_body(ClientSocket::socket(),atom(),
%%                    Headers::headers(),AppConfig::app_config())
%%            -> string()
%% @doc
%% We look for a body for all modes except get; there must
%% be a content length in the header so we know where the
%% body content ends; we might lose patience if it takes
%% too long to read it in, in which case return nothing at a blank body.
%% @end
%%--------------------------------------------------------------------
read_request_body(_ClientSocket,get, _Headers,_AppConfig)-> 
    [];
read_request_body(ClientSocket,_, Headers,AppConfig) 
  when is_list(Headers),
       is_record(AppConfig,app_config)-> 

    ContentLength = pranayama_utils:figure_content_length(Headers),
    case ContentLength > 0 of
	true->

	    case pranayama_net:receive_raw_data(ClientSocket,ContentLength,AppConfig#app_config.receive_request_timeout_ms) of 
		
		{ok,Body}->
		    Body;

		Otherwise ->
		    pranayama_logger:warn("Connection failed to read the posted body. Possible error was:~p",[Otherwise]),
		    []
	    end;
	false->
	    pranayama_logger:trace("Posted, but with zero for Content-Length"),
	    []
   end.

%%--------------------------------------------------------------------
%% @spec enhance(HttpRequest::http_request())->http_request()
%% @doc
%% As mentioned above, we supplement the request with some derived info.
%%
%% @end
%%--------------------------------------------------------------------
enhance(HttpRequest) when is_record(HttpRequest,http_request)->
    
    HttpRequest#http_request{web_app_alias=pranayama_utils:figure_web_app_alias(HttpRequest),
			     web_app_path=pranayama_utils:figure_web_app_path(HttpRequest),
			     params = pranayama_utils:figure_normalized_params(HttpRequest),
			     response_format=pranayama_utils:figure_response_format(HttpRequest),
			     client_description=pranayama_utils:figure_client_description(HttpRequest)
			    }.
    
%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

%% @hidden
start_to_read_request_test()->

    AppConfig = #app_config{},
    ClientSocket = [],
    
    pranayama_net:set_mock_response(receive_data,{ok,{http_request,"GET",{undefined,"some_app/somepath"},{1,1}}}),
    pranayama_net:set_mock_response(get_client_info,{ok,{{192,168,1,1},6464}}),
    
    ?assertMatch(#http_request{method=get,uri="some_app/somepath",version={1,1},client_ip={192,168,1,1},client_port=6464,_=_},
		     start_to_read_request(AppConfig,ClientSocket)),

    pranayama_net:set_mock_response(receive_data,{error,einval}),
    ?assertThrow(failed_to_read_request,start_to_read_request(AppConfig,ClientSocket)),

    ok.

%% @hidden
read_request_headers_test()->

    ClientSocket = [],

    ReturnedData = [{ok, {http_header, undefined, "FirstKey",undefined, "FirstValue"}},
		    {ok, {http_header, undefined, second_key,undefined, "SecondValue"}},
		    {error, {http_error,"Some Http Error Which We Ignore"}},
		    {ok, {http_header, undefined, 'ThirdKey',undefined, "ThirdValue"}},
		    {ok, http_eoh}],
    
    pranayama_net:set_mock_response(receive_data,{stream,ReturnedData}),
    ?assertMatch([{"FirstKey","FirstValue"},
		  {"second_key","SecondValue"},
		  {"ThirdKey","ThirdValue"}
		 ],
		 read_request_headers(ClientSocket)),

    ok.
    
%% @hidden
read_request_body_test()->

    ClientSocket = [],
    Headers = [{"Content-Length","100"}],
    AppConfig = #app_config{},

    ReturnedBody = "Afternoon",

    pranayama_net:set_mock_response(receive_raw_data,{ok,ReturnedBody}),
    ?assertMatch(ReturnedBody,read_request_body(ClientSocket,post,Headers,AppConfig)),

    pranayama_net:set_mock_response(receive_raw_data,{"Something Else Happened"}),
    ?assertMatch([],read_request_body(ClientSocket,post,Headers,AppConfig)),

    ?assertMatch([],read_request_body(ClientSocket,post,[],AppConfig)),

    ?assertMatch([],read_request_body(ClientSocket,get,Headers,AppConfig)),
    ?assertMatch([],read_request_body(ClientSocket,get,[],AppConfig)),
    

    
    ok.

%% @hidden
read_get_test()->

    AppConfig = #app_config{},
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
    
    pranayama_net:set_mock_response(receive_data,{stream,ReceiveDataResponse}),
    pranayama_net:set_mock_response(get_client_info,{ok,{ClientIP,ClientPort}}),

    HttpRequest = read(AppConfig,ClientSocket),
    ?assert(is_record(HttpRequest,http_request)),
    ?assertEqual(get,HttpRequest#http_request.method),
    ?assertEqual(URI,HttpRequest#http_request.uri),
    ?assertEqual(Version,HttpRequest#http_request.version),
    ?assertEqual(ClientIP,HttpRequest#http_request.client_ip),
    ?assertEqual(ClientPort,HttpRequest#http_request.client_port),
    ?assertEqual(undefined,HttpRequest#http_request.client_username),
    ?assertEqual([],HttpRequest#http_request.body),
    ?assertEqual(WebAppAlias,HttpRequest#http_request.web_app_alias),
    ?assertEqual(WebAppPath,HttpRequest#http_request.web_app_path),
    ?assertEqual([{"fname",["Nina"]},{"lname",["Simone"]}],HttpRequest#http_request.params),
    ?assertEqual(ClientDescription,HttpRequest#http_request.client_description),
    
    ok.

%% @hidden
read_post_test()->

    AppConfig = #app_config{},
    ClientSocket = [],

    WebAppAlias = "some_app",
    WebAppPath = "somepath",
    URI = WebAppAlias ++ "/" ++ WebAppPath,
    Version = {1,1},
    ClientIP = {192,168,1,1},
    ClientPort = 6464,
    ClientDescription = "JustLookingThanks 0.4",

    ReceiveDataResponse = [{ok,{http_request,"POST",{undefined,URI},Version}},
		    {ok, {http_header, undefined, "Content-Length",undefined, "1000"}},
		    {ok, {http_header, undefined, "User-Agent",undefined, ClientDescription }},	   
		    {ok, http_eoh}],
    
    pranayama_net:set_mock_response(receive_data,{stream,ReceiveDataResponse}),
    pranayama_net:set_mock_response(get_client_info,{ok,{ClientIP,ClientPort}}),

    ReturnedBody = "fname=Nina&lname=Simone",

    pranayama_net:set_mock_response(receive_raw_data,{ok,ReturnedBody}),

    HttpRequest = read(AppConfig,ClientSocket),
    ?assert(is_record(HttpRequest,http_request)),
    ?assertEqual(post,HttpRequest#http_request.method),
    ?assertEqual(URI,HttpRequest#http_request.uri),
    ?assertEqual(Version,HttpRequest#http_request.version),
    ?assertEqual(ClientIP,HttpRequest#http_request.client_ip),
    ?assertEqual(ClientPort,HttpRequest#http_request.client_port),
    ?assertEqual(undefined,HttpRequest#http_request.client_username),
    ?assertEqual(ReturnedBody,HttpRequest#http_request.body),
    ?assertEqual(WebAppAlias,HttpRequest#http_request.web_app_alias),
    ?assertEqual(WebAppPath,HttpRequest#http_request.web_app_path),
    ?assertEqual([{"fname",["Nina"]},{"lname",["Simone"]}],HttpRequest#http_request.params),
    ?assertEqual(ClientDescription,HttpRequest#http_request.client_description),
    
    ok.
