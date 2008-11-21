%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% A web app that has a privileged view into a running Pranayama.
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
%%% @todo Document
%%%-------------------------------------------------------------------


-module(pranayama_spy).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").

-define(SERVER,?MODULE).

-record(state,{app_config,
	       web_app_config}).

%%====================================================================
%% API
%%====================================================================
start_link(AppConfig) when is_record(AppConfig,app_config) ->

    WebAppConfig = #web_app_config{alias=spy,global_process_name=?SERVER},

    gen_server:start_link({global, ?SERVER}, ?MODULE, [AppConfig,WebAppConfig], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================


init([AppConfig,WebAppConfig]) 
  when is_record(AppConfig,app_config),
       is_record(WebAppConfig,web_app_config) ->
    
    case gen_server:call({global,pranayama_registrar},
		    {register,WebAppConfig}) of
	{register_response,ok}->
	    pranayama_logger:info("Pranayama Spy has successfully registered.");
	Otherwise ->
	    exit("Failed to register Pranayama Spy:~p",[Otherwise])
    end,

    {ok, #state{app_config=AppConfig,web_app_config=WebAppConfig}}.

handle_call({generate_response,HttpRequest}, _From, State) when is_record(HttpRequest,http_request)->
    
    Reply = generate_response(State#state.app_config,
			      HttpRequest),
			      
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

generate_response(AppConfig,#http_request{method=get,web_app_path="echo",_=_}=HttpRequest)
    when is_record(AppConfig,app_config),
	 is_record(HttpRequest,http_request)
	 ->
    
    #http_response{status=ok,
		   content_type=html,
		   body=io_lib:format("<pre>Request: ~n~p~n</pre>",[HttpRequest])};


generate_response(AppConfig,#http_request{method=get,web_app_path="ping",_=_}=HttpRequest)
    when is_record(AppConfig,app_config),
	 is_record(HttpRequest,http_request)
	 ->

    ConfigReport = lists:concat([
				 
				 "Port:",
				 integer_to_list(AppConfig#app_config.port_num),
				 "<br/>",
				 "Web App Registration Process Name:",
				 AppConfig#app_config.web_app_registration_process_name,
				 "<br/>",
				 "Receive Request Timeout (ms):",
				 integer_to_list(AppConfig#app_config.receive_request_timeout_ms),
				 "<br/>",
				 "Handle Request Timeout (ms):",
				 integer_to_list(AppConfig#app_config.handle_request_timeout_ms),
				 "<br/>",
				 "Log Directory:",
				 AppConfig#app_config.log_directory,
				 "<br/>"
				]),
    
    #http_response{status=ok,
		   content_type=html,
		   body=io_lib:format("<html><body bgcolor='lightyellow'><h3>Pranayama</h3><hr size=1/>~s<hr size=1/></body></html>",
				      [ConfigReport])};

generate_response(AppConfig,HttpRequest)
    when is_record(AppConfig,app_config),
	 is_record(HttpRequest,http_request)->
    
    pranayama_logger:trace("What?"),

    #http_response{status=ok,
		   content_type=html,
		   body=io_lib:format("<pre>Cannot handle this Request:~nMethod(~p)~nPath(~p)~nResponse Format(~p)~n</pre>",
				      [HttpRequest#http_request.method,HttpRequest#http_request.web_app_path,HttpRequest#http_request.response_format])}.

    
	
