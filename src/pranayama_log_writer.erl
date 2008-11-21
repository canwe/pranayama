%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%  Responsible for writing out the Access log
%%%  the format of which is the Combined Log format described
%%%  here - [http://httpd.apache.org/docs/1.3/logs.html#combined].
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
-module(pranayama_log_writer).

-behaviour(gen_event).

-export([start_link/0]).

-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").

-define(SERVER,?MODULE).
-define(LOG_FILE_NAME,"pranayama_access.log").

%% @type access_log_entry() = #app_log_entry{client_ip=ip(),
%%                                          client_username=string(),     
%%                                          client_description=string(),
%%                                          datetime=string(),
%%                                          request_summary=string(),  
%%                                          response_status_code=integer(),  
%%                                          response_content_length=integer()
%%                                          }.
%%                                             
%% === datetime ===
%% Format: [day/month/year:hour:minute:second zone]
%% Example: 10/Oct/2000:13:55:36 -0700 
%% 
%% === request_summary ===
%% Example: "(Method Path HTTP/Version)" - "GET /apache_pb.gif HTTP/1.0"
-record(access_log_entry,
	{
	  client_ip,
	  client_username,
	  client_description,
	  datetime, 
	  request_summary, 
	  response_status_code, 
	  response_content_length 
	  }).

%%--------------------------------------------------------------------
%% @type state() = #state{access_file_handle=file(),
%%                        server_timezone_offset=string()
%%                       }.
%%
%% === server_timezone_offset ===
%% The number of hours difference between local and
%% universal time; for instance, EST is "-5.00"; if
%% the local time was after universal time, we prefix
%% with a plus; for example Greece (EET) would be "+2.00"
%%
%% @end
%%--------------------------------------------------------------------
-record(state, {access_file_handle,
		server_timezone_offset}).


%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid::pid()} | {error, Error::term()}
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_link()->
    case gen_event:start_link({local,?SERVER}) of
	{ok,Pid}=Response->
	    pranayama_logger:info("Started Log Writer: ~p",[Pid]),
	    Response;
	{error,Error}=Response->
	    pranayama_logger:error("Failed to start Log Writer:~p",[Error]),
	    Response
    end.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec init(_Args::[AppConfig::app_config()]) -> {ok, State::state()}
%% @doc
%% Append access to the log file is essential so we exit otherwise;
%% note that we will close this file in 'terminate' which is triggered
%% when we are deregistered from pranayama_event_manager.
%%
%% <i>"Whenever a new event handler is added to an event manager, this
%% function is called to initialize the event handler."</i>
%%
%% @end
%% --------------------------------------------------------------------
init([AppConfig]=_Args) when is_record(AppConfig,app_config) ->    

    ServerTimezoneFormat = figure_server_timezone_offset(),


    LogFile = AppConfig#app_config.log_directory ++ "/" ++ ?LOG_FILE_NAME,
    case file:open(LogFile,[append]) of
	
	{ok,AccessFileHandler}-> 
	    
	    {ok, #state{access_file_handle=AccessFileHandler,
		server_timezone_offset=ServerTimezoneFormat}};
	{error,Error} ->
	    pranayama_logger:error("Failed to open access log (~p) for append. Reason:~p",
				   [LogFile,Error]),
	    exit({failed_to_open_access_log_file,LogFile})
    end.

%%--------------------------------------------------------------------
%% @spec handle_event(Event, State::state()) ->
%%                          {ok, State::state()} |
%%                          {swap_handler, Args1, State::state(), Mod2::atom(), Args2} |
%%                          remove_handler
%% @doc
%% We currently respond to one event: what we presume to be a newly 
%% delivered #http_response. There is no guard against logging the
%% same Response twice, so it should only be sent once.
%%
%% We take care not to log anything for the 'log' event or
%% we'll wind up in an infinite loop.
%%
%% <i>"Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event."</i>
%%
%% @end
%% --------------------------------------------------------------------
handle_event({response,HttpResponse}, State) when is_record(HttpResponse,http_response) ->
    log_access_event(HttpResponse,State#state.access_file_handle,State#state.server_timezone_offset),
    {ok, State};

handle_event({log,LogMessage}, State) when is_record(LogMessage,log_message) ->
    {ok,State};

handle_event(Event, State) ->
    pranayama_logger:trace("The Log Writer was sent an Event that is doesn't deal with: ~p",
			   [Event]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(_Request::term(), State::state) ->
%%                   {ok, Reply::term(), State::state()} |
%%                   {swap_handler, Reply::term(), Args1, State1::state(), Mod2::atom(), Args2} |
%%                   {remove_handler, Reply::term()}
%% @doc
%% We don't expect any calls, so we simply log it, and reply with ok.
%%
%% <i>"Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request."</i>
%%
%% @end
%% --------------------------------------------------------------------
handle_call(Request, State) ->

     pranayama_logger:trace("The Log Writer was sent a call that is didn't expect:~p",
			   [Request]),

    
    {ok, ok, State}.


%%--------------------------------------------------------------------
%% @spec handle_info(Info::term(), State::state()) ->
%%                         {ok, State::state()} |
%%                         {swap_handler, Args1, State1::state(), Mod2::atom(), Args2} |
%%                         remove_handler
%% @doc
%% We don't expect any info messages, so we simply log it, and reply with ok.
%%
%% <i>"This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message)."</i>
%%
%% @end
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @spec terminate(Reason, State::state()) -> none()
%% @doc
%% We close the file handle for our access log file, which should
%% have been opened in init/1
%%
%% <i>"Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up."</i>
%%
%% @end
%% --------------------------------------------------------------------
terminate(Reason, State) ->

    pranayama_logger:trace("Terminating Log Writer:~p",[Reason]),
    
    file:close(State#state.access_file_handle),

    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc
%% <i>"Convert process state when code is changed"</i>
%%
%% @end
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec log_access_event(HttpResponse::http_response,FileHandle::file(),
%%                        ServerTimeZoneOffset::string())
%%                        -> ok
%% @doc
%%
%%
%% @end
%% --------------------------------------------------------------------
log_access_event(HttpResponse,FileHandle,ServerTimeZoneOffset)
  when is_record(HttpResponse,http_response)->

    AccessLogEntry = generate_access_log_entry(HttpResponse,ServerTimeZoneOffset),
    write(AccessLogEntry,FileHandle),
    ok.

%%--------------------------------------------------------------------
%% @spec generate_access_log_entry(HttpResponse::http_response,
%%                        ServerTimeZoneOffset::string())
%%                        -> access_log_entry()
%% @doc
%%
%%
%% @end
%% --------------------------------------------------------------------
generate_access_log_entry(HttpResponse,ServerTimeZoneOffset)
  when is_record(HttpResponse,http_response)->

    #access_log_entry{
	  client_ip = prepare_client_ip(HttpResponse#http_response.client_ip),
	  client_username = default(HttpResponse#http_response.client_username),
	  client_description = default(HttpResponse#http_response.client_description),									     
	  datetime = format_datetime(calendar:local_time(),ServerTimeZoneOffset), 
	  request_summary = generate_request_summary(HttpResponse), 
	  response_status_code=HttpResponse#http_response.status_code_num,
	  response_content_length=default(HttpResponse#http_response.content_length)
	  }.

%%--------------------------------------------------------------------
%% @spec prepare_client_ip(IP::ip()) -> string()
%% @doc
%% Format it to resemble: "192.168.1.200" or whatever IP6 looks like
%% (2001:0db8:0000:0000:0000:0000:1428:57ab)
%%
%% @end
%% --------------------------------------------------------------------
prepare_client_ip(IP) when is_tuple(IP)->
    string:join([integer_to_list(Part) || Part <- tuple_to_list(IP)],".").


%%--------------------------------------------------------------------
%% @spec default(any()) -> any()
%% @doc
%% The log format requires that we sometimes provide a '-' in lieu of
%% a blank.
%%
%% @end
%% --------------------------------------------------------------------
default(undefined)->
    "-";
default(0)->
    "-";
default(Something)->
    Something.


%%--------------------------------------------------------------------
%% @spec generate_request_summary(HttpResponse::http_response())
%%          ->string()
%% @doc
%% Produces the #access_log_entry.request_summary. 
%%
%% @end
%% --------------------------------------------------------------------
generate_request_summary(HttpResponse)
  when is_record(HttpResponse,http_response)->

    lists:flatten(lists:concat(
		  ['"',
		  string:to_upper(atom_to_list(HttpResponse#http_response.request_method)),
		  " ",
		  HttpResponse#http_response.requested_full_path,
		  " ",
		  "HTTP/",
		  prepare_version(HttpResponse#http_response.client_version),
		  '"'])).


%%--------------------------------------------------------------------
%% @spec prepare_version(any())-> string()
%% @doc
%% Originally, we should have an http protocol version like {1,1};
%% we want to convert this into "1.1"; we default to "?" in the
%% failing case.
%%
%% @end
%% @todo Needs testing
%% --------------------------------------------------------------------
prepare_version(undefined)->
    "?";
prepare_version(Version) when is_tuple(Version) ->

    ListOfStrings = [integer_to_list(VersionPart)|| VersionPart <- tuple_to_list(Version)],
    string:join(ListOfStrings,".");

prepare_version(Version) when is_list(Version) ->
    Version;
prepare_version(_Version) ->
    "?".

%%--------------------------------------------------------------------
%% @spec figure_server_timezone_offset()->string()
%% @doc
%% We should only have to do this once (unless our server's timezone
%% mysteriously changes in mid-flight); this provides the
%% #state.server_timezone_offset, described above.
%%
%% @end
%% --------------------------------------------------------------------
figure_server_timezone_offset()->

    LocalTimeInSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    UniversalTimeInSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    DiffInHours = (LocalTimeInSeconds - UniversalTimeInSeconds) / 60 /60,
   
    [RoundedDiff] = io_lib:format("~.2f",[DiffInHours]),
    case list_to_float(RoundedDiff) > 0 of
	true->
	    lists:concat(["+",RoundedDiff]);
	false->
	 RoundedDiff
    end.

%%--------------------------------------------------------------------
%% @spec format_datetime(_DateTime::datetime(),
%%                       ServerTimeZoneOffset::string())->string()
%% @doc
%% Prepares #state.datetime, described above.
%%
%% @end
%% --------------------------------------------------------------------
format_datetime({{Year,Month,Day},{Hour,Minute,Second}}=_DateTime,ServerTimeZoneOffset)->

    lists:concat([
		  "[",
		  Day,
		  "/",
		  httpd_util:month(Month),
		  "/",
		  Year,
		  ":",
		  Hour,
		  ":",
		  Minute,
		  ":",
		  Second,
		  " ",
		  ServerTimeZoneOffset,
		  "]"
		 ]).

%%--------------------------------------------------------------------
%% @spec write(AccessLogEntry::access_log_entry(),
%%             FileHandle::file())->ok
%% @doc
%% RFC 1413 is the 'identification protocol' which we don't currently
%% support (and probably won't as it seems v. complicated with low
%% returns). [http://en.wikipedia.org/wiki/Ident]
%%
%% @end
%% --------------------------------------------------------------------
write(AccessLogEntry,FileHandle) when is_record(AccessLogEntry,access_log_entry)->

    AccessEntryLine = lists:concat([AccessLogEntry#access_log_entry.client_ip,

				    " ",
				    AccessLogEntry#access_log_entry.client_username,

				    " ",
				    "-", %% RFC 1413 not supported
				    
				    " ",
				    AccessLogEntry#access_log_entry.datetime,

				    " ",
				    AccessLogEntry#access_log_entry.request_summary,

				    " ",
				    AccessLogEntry#access_log_entry.response_status_code,

				    " ",
				    AccessLogEntry#access_log_entry.response_content_length,

				    " ",
				    "-", % Referrer not supported

				    " ",
				    '"',
				    AccessLogEntry#access_log_entry.client_description,
				    '"'
				    
				    
				   ]),
    io:format(FileHandle,"~s~n",[AccessEntryLine]),
    ok.
   



