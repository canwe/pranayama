%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% Responsible for generating and sending back the Http Request
%%% to the client; the response generation is delegated to
%%% the appropriate web app, of course.
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
-module(pranayama_response_writer).


-export([write/3]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec write(AppConfig::app_config(),HttpRequest::http_request(),
%%             ClientSocket::socket())
%%            -> http_response() 
%% @doc
%% Note that we enhance the http_response returned from the delegate.
%%
%% @end
%%--------------------------------------------------------------------
write(AppConfig,HttpRequest,ClientSocket)
  when is_record(AppConfig,app_config),
       is_record(HttpRequest,http_request)->

    HttpResponse = enhance(generate_response(AppConfig,HttpRequest),HttpRequest),
    
    RealHttpResponse = generate_real_http_response(HttpResponse),
   
    send_response(ClientSocket,RealHttpResponse),
    HttpResponse.


    
%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec generate_response(AppConfig::app_config(),HttpRequest::http_request())
%%            -> http_response() 
%% @doc
%% Delegation to the appropriate web app.
%%
%% @end
%%--------------------------------------------------------------------
generate_response(AppConfig,HttpRequest) 
  when is_record(AppConfig,app_config),
       is_record(HttpRequest,http_request)->

    case pranayama_net:call_global_process(AppConfig#app_config.web_app_registration_process_name,
			 {get_web_app_global_name,HttpRequest#http_request.web_app_alias}) of

	{get_web_app_global_name_response,undefined}->
	
	    pranayama_logger:error("Failed to find web app for alias (~p)",
				   [HttpRequest#http_request.web_app_alias]),
	    #http_response{status=not_found,
			   content_type=text,
			   body=lists:concat(["No webapp registered for alias:",HttpRequest#http_request.web_app_alias])
			  };
	{get_web_app_global_name_response,WebAppGlobalName} when is_atom(WebAppGlobalName)->
	    pranayama_logger:trace("Found global name (~p) for alias (~p)",[WebAppGlobalName,HttpRequest#http_request.web_app_alias]),
	    pranayama_net:call_global_process(WebAppGlobalName,{generate_response,HttpRequest});

	Unexpected ->
	    pranayama_logger:error("Failed to find web app for alias - Unexpected Response(~p)",
				   [Unexpected]),
	    #http_response{status=not_found,
			   content_type=text,
			   body=lists:concat(["No webapp registered for alias:",HttpRequest#http_request.web_app_alias])
			  }
    

    end.

%%--------------------------------------------------------------------
%% @spec generate_real_http_response(HttpResponse::http_response())
%%            -> list() 
%% @doc
%% Conversion into the 'real' Http Response 
%%
%% @end
%%--------------------------------------------------------------------
generate_real_http_response(HttpResponse) 
  when is_record(HttpResponse,http_response),
       HttpResponse#http_response.body=:=undefined ->
    
    generate_real_http_response(HttpResponse#http_response{body=[]});

generate_real_http_response(HttpResponse) 
  when is_record(HttpResponse,http_response),
       is_list(HttpResponse#http_response.body)->
    
    generate_real_http_response(
      HttpResponse#http_response{body=list_to_binary(HttpResponse#http_response.body)});

generate_real_http_response(HttpResponse) 
  when is_record(HttpResponse,http_response),
       is_binary(HttpResponse#http_response.body)->
    
    ResponseLines = 
	[
	 "HTTP/1.0 ",HttpResponse#http_response.status_code_num," ",HttpResponse#http_response.status_code_text,"\r\n",
	 "Content-type:",figure_content_type(HttpResponse),"\r\n",
	 "Content-length:", length(binary_to_list(HttpResponse#http_response.body)),"\r\n",
	 "\r\n",
         binary_to_list(HttpResponse#http_response.body)
	 
	 
	],
    
    lists:concat(ResponseLines).   

%%--------------------------------------------------------------------
%% @spec send_response(ClientSocket::socket(),RealHttpResponse::list())
%%            -> any() 
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
send_response(ClientSocket,RealHttpResponse) 
  when is_list(RealHttpResponse)->

  pranayama_net:send_data(ClientSocket,RealHttpResponse).  

%%--------------------------------------------------------------------
%% @spec figure_content_type(HttpResponse::http_response())
%%            -> string()
%% @doc
%%   Returns something like "text/xml"
%% @end
%%--------------------------------------------------------------------
figure_content_type(HttpResponse)
  when is_record(HttpResponse,http_response)->

    pranayama_utils:search_for_tuple_value(HttpResponse#http_response.content_type,
					   ?CONTENT_TYPES,
					   ?FALLBACK_CONTENT_TYPE).

%%--------------------------------------------------------------------
%% @spec enhance(HttpResponse::http_response(),HttpRequest::http_request())
%%            -> string()
%% @doc
%%   We decorate the http_response with info that will be useful to us
%%   down the road (for instance, with the access logger).
%% @end
%%--------------------------------------------------------------------
enhance(HttpResponse,HttpRequest)
  when is_record(HttpResponse,http_response),
       is_record(HttpRequest,http_request)->

    {StatusCodeNum,StatusCodeText} = figure_status_codes(HttpResponse#http_response.status),

    Body = pranayama_utils:fallback(HttpResponse#http_response.body,""),
    
    #http_response{status=HttpResponse#http_response.status,
		   status_code_num=StatusCodeNum,
		   status_code_text=StatusCodeText,
		   content_type=HttpResponse#http_response.content_type,
		   body=Body,
		   client_ip=HttpRequest#http_request.client_ip,
		   client_username=HttpRequest#http_request.client_username,
		   client_version=HttpRequest#http_request.version,
		   client_description=HttpRequest#http_request.client_description,
		   request_method=HttpRequest#http_request.method,
		   requested_full_path=HttpRequest#http_request.uri,
		   content_length=figure_body_length(Body)
		  }.

%%--------------------------------------------------------------------
%% @spec figure_body_length(any())
%%            -> integer()
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
figure_body_length(undefined)->
    0;
figure_body_length(Body) when is_binary(Body) ->
    length(binary_to_list(Body));
figure_body_length(Body) when is_list(Body) ->
    length(Body);
figure_body_length(Body) ->
    pranayama_logger:error("What kind of Body is this? (~p)",[Body]),
    0.

%%--------------------------------------------------------------------
%% @spec figure_status_codes(Status::atom())
%%            -> tuple()
%% @doc
%%  Looking for something like {200,"OK"}.
%% @end
%%--------------------------------------------------------------------
figure_status_codes(Status)
  when is_atom(Status)->

    pranayama_utils:search_for_tuple_value(Status,
					   ?STATUS_CODES,
					   ?FALLBACK_STATUS_CODE).


 

%%%===================================================================
%%% Test Functions.
%%%===================================================================

%% @private
generate_real_http_response_test()->

    RealHttpResponse = generate_real_http_response(#http_response{status_code_num=200,status_code_text="OK"}),
    ?debugVal(RealHttpResponse),
    ?assert(is_list(RealHttpResponse)),
    ok.

%% @private
figure_content_type_test()->

    ?assertEqual("application/json",figure_content_type(#http_response{content_type=json})),
    ?assertEqual("text/xml",figure_content_type(#http_response{content_type=xml})),
    ?assertEqual(?FALLBACK_CONTENT_TYPE,figure_content_type(#http_response{content_type=wtf})),
    
    ok.

%% @private
figure_status_codes_test()->

    ?assertEqual({200,"OK"},figure_status_codes(ok)),
    ?assertEqual({404,"Not Found"},figure_status_codes(not_found)),
    ?assertEqual(?FALLBACK_STATUS_CODE,figure_status_codes(wtf)),

    ok.
 
