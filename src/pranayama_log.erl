%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%  Responsible for general logging - not to be confused
%%%  with the webserver style logging found in
%%%  <em>pranayama_log_writer</em>; there are various levels
%%%  of detail; currently, output is only to a file.
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
%%% @todo Use an existing lib like log4e? Good to have thresholds.
%%%-------------------------------------------------------------------
-module(pranayama_log).

-behaviour(gen_event).

-include("../include/pranayama_internal.hrl").

-export([start_link/0]).

-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(LOG_HEADER_TRACE,"[TRACE]: ").
-define(LOG_HEADER_INFO,"[INFO]: ").
-define(LOG_HEADER_WARN,"[WARN]: ").
-define(LOG_HEADER_ERROR,"[ERROR]: ").

-define(SERVER,?MODULE).
-define(LOG_FILE_NAME,"pranayama.log").

%% @type state() = #state{app_config=app_config(),
%%                        log_file_handle=file()
%%                        }.
-record(state, {app_config,
		log_file_handle}).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}
%% @doc
%% The skeleton says "Creates an event manager" but that confuses me:
%% aren't we creating an event *handler*; in any case <em>pranayama_sup</em>,
%% needs to call something to start this process.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec init(_Args::[AppConfig::app_config()]) -> {ok, State}
%% @doc
%%  We open our log file, the path of which is included in the
%%  #app_config; if we fail we log the error but we do not exit, as
%%  it's not an essential service.
%%
%% <i>"Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler."</i>
%%
%% @end
%%--------------------------------------------------------------------
init([AppConfig]=_Args) when is_record(AppConfig,app_config) ->

    LogFile = AppConfig#app_config.log_directory ++ "/" ++ ?LOG_FILE_NAME,
    
    LogFileHandle =
	case file:open(LogFile,[append]) of
	    {ok,_LogFileHandle}->
		_LogFileHandle;
	    _Error->
	       io:format("Failed to open LogFile (~p) because (~p).~n",[LogFile,_Error]),
	       undefined
	end,
        
    {ok, #state{app_config=AppConfig,log_file_handle=LogFileHandle}}.

%%--------------------------------------------------------------------
%% @spec handle_event(Event::term(), State::state()) ->
%%                          {ok, State::state()} 
%% @doc
%% There is only one event that interests us and that is the request
%% to log.
%%
%% <i>"Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event."</i>
%%
%% @end
%%--------------------------------------------------------------------
handle_event({log,LogMessage}, State)
  when is_record(LogMessage,log_message),
       is_record(State,state) ->

    log(State,LogMessage),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @spec handle_call(Request::term(), State::state()) ->
%%                   {ok, ok, State::state()} 
%% @doc
%% We expect no calls.
%% 
%%
%% <i>"Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request."</i>
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State)
  when is_record(State,state) ->
    {ok, ok, State}.



%%--------------------------------------------------------------------
%% @spec handle_info(Info::term(), State::state()) ->
%%                         {ok, State::state()} 
%% @doc
%% We expect no info.
%% 
%%
%% <i>"This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message)."</i>
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc
%% We close the file handle, if we have one.
%% 
%%
%% <i>"Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up."</i>
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->

    case State#state.log_file_handle=/=undefined of
	true->
	    file:close(State#state.log_file_handle);
	false ->
	    ok
    end,

    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc
%% We close the file handle, if we have one.
%% 
%% <i>"Convert process state when code is changed."</i>
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec log(State::state(),LogMessage::log_message())-> ok
%% @doc
%% 
%%
%% @end
%%--------------------------------------------------------------------
log(State,LogMessage)
  when is_record(State,state),
       is_record(LogMessage,log_message)->
    log(State,figure_header(LogMessage),LogMessage#log_message.template,LogMessage#log_message.params).


%%--------------------------------------------------------------------
%% @spec figure_header(LogMessage::log_message())-> string()
%% @doc
%% For every log level, there is a corresponding prefix or header. 
%% 
%%
%% @end
%%--------------------------------------------------------------------
figure_header(LogMessage)
  when is_record(LogMessage,log_message)->

    case LogMessage#log_message.level of
	trace->
	    ?LOG_HEADER_TRACE;
	info->
	    ?LOG_HEADER_INFO;
	warn->
	    ?LOG_HEADER_WARN;
	error->
	    ?LOG_HEADER_ERROR;
	_->
	    "[?]"
    end.


%%--------------------------------------------------------------------
%% @spec log_to_file(Root::string(),Param::list(),State::state())-> ok
%% @doc
%% 
%%
%% @end
%%--------------------------------------------------------------------
log_to_file(Root,Params,State) when is_record(State,state)->

    case State#state.log_file_handle=/=undefined of
	true->	   
	    io:format(State#state.log_file_handle,Root,Params),
	    ok;
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec log(State::state(),Header::string(),
%%           Template::string(),Params::list())-> ok
%% @doc
%% You'll note that we absorb any exceptions (not an essential service). 
%%
%% @end
%% @todo Add option to log to stdout and other 'channels' one day
%% like XMPP.
%%--------------------------------------------------------------------
log(State,Header,Template,Params)
  when is_record(State,state)->

    Root = pranayama_utils:pad(Header,10) ++ "(" ++ pid_to_list(self()) ++ ") [" ++ httpd_util:rfc1123_date() ++ "] " ++ Template ++ "~n",
    
    catch(log_to_file(Root,Params,State)),

    ok.

    
