%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%  Currently, this is the only OTP supervisor, although
%%%  pranayama_event_manager_sup behaves like one.
%%%  
%%%  The order in which the children are loaded does matter as you'll
%%%  read below (init/1).
%%%
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
%%% Created : 14 Nov 2008
%%%-------------------------------------------------------------------

-module(pranayama_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-include("../include/pranayama_internal.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec start_link(AppConfig::app_config()) -> {ok,Pid} | ignore | {error,Error}
%% @doc
%%
%%  <i>"Starts the supervisor"</i>
%%
%% @end
%%--------------------------------------------------------------------
start_link(AppConfig) when is_record(AppConfig,app_config) ->
    supervisor:start_link(?MODULE, [AppConfig]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init([AppConfig::app_config()]) ->
%%          {ok,  {SupFlags,  [ChildSpec]}} | ignore | {error, Reason}
%% @doc
%% You'll notice that we take care to put the event related children
%% at the beginning; we need to do this as they include the
%% pranayama_log - a gen_event - which is used by nearly all other
%% modules; for this reason you cannot log anything before this init
%% completes.
%%
%% The Spy modules isn't a required part of Pranayama: it is provided
%% for administrative convenience as well as an example of a web app;
%% however, you'll note that it has a <em>privilaged</em> relation to the
%% Server in that it receives the #app_config, as well as being able
%% to directly call any of the Server modules.
%%
%% For all the children, we will allow 10 seconds to terminate
%% normally before such termination is forced; each child may
%% be restarted a maximum of 3 times within 10 seconds before
%% our entire app exits.
%%
%% <i>"Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new
%% process to find out about restart strategy, maximum restart
%% frequency and child specifications."</i>
%%
%% @end
%%--------------------------------------------------------------------

init([AppConfig]) when is_record(AppConfig,app_config) ->

    MaxRestartsWithinRestartPeriod = 3,
    RestartPeriodInSeconds = 10,

    MaxTimeToAllowForTerminationInMs=10000,
    
    
    EventManager = 
	{pranayama_event_manager,
	 {pranayama_event_manager,start_link,[]}, 
	 permanent,
	 MaxTimeToAllowForTerminationInMs, 
	 worker,
	 [pranayama_event_manager]},
    
    EventManagerSupervisor = 
	{pranayama_event_manager_sup,
	 {pranayama_event_manager_sup,start_link,[AppConfig]}, 
	 permanent,
	 MaxTimeToAllowForTerminationInMs, 
	 worker,
	 [pranayama_event_manager_sup]},

    Log =
 	{pranayama_log,
 	 {pranayama_log,start_link,[]}, 
 	 permanent,
 	 MaxTimeToAllowForTerminationInMs, 
 	 worker,
 	 [pranayama_log]},
    
    LogWriter =
 	{pranayama_log_writer,
 	 {pranayama_log_writer,start_link,[]}, 
 	 permanent,
 	 MaxTimeToAllowForTerminationInMs, 
 	 worker,
 	 [pranayama_log_writer]},
    
    Conversationalist = 
	{pranayama_conversationalist,
	 {pranayama_conversationalist,start_link,[AppConfig]}, 
	 permanent,
	 MaxTimeToAllowForTerminationInMs, 
	 worker,
	 [pranayama_conversationalist]},

    Registrar = 
	{pranayama_registrar,
	 {pranayama_registrar,start_link,[AppConfig]}, 
	 permanent,
	 MaxTimeToAllowForTerminationInMs, 
	 worker,
	 [pranayama_registrar]},

    
    Spy = 
	{pranayama_spy,
	 {pranayama_spy,start_link,[AppConfig]}, 
	 permanent,
	 MaxTimeToAllowForTerminationInMs, 
	 worker,
	 [pranayama_spy]},


    EventRelatedModules = [EventManager,EventManagerSupervisor,Log,LogWriter],
    AllModules = lists:flatten([EventRelatedModules,Conversationalist,Registrar,Spy]),
    
    {ok,{
       {one_for_one,
	 MaxRestartsWithinRestartPeriod, 
 	 RestartPeriodInSeconds
	},
     AllModules}}.



%%====================================================================
%% Internal functions
%%====================================================================
