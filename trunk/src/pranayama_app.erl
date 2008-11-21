%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%  This is the launch point of the application; we read in the
%%%  config settings from the pranayama.conf file and start the top-most
%%%  supervisor - pranayama_sup. The location of this conf file is
%%%  set in ebin/pranayama.app.
%%%
%%%  The application configuration is represented by the #app_config
%%%  record, and is passed throughout the system.
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
-module(pranayama_app).

%% @todo How to suppress the behaviour link?
-behaviour(application).

-export([start/2, stop/1]).

-include("../include/pranayama_internal.hrl").

-define(APPLICATION,pranayama).

%%====================================================================
%% Application callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start(Type::atom(),StartArgs::list()) ->
%%                       {ok,Pid} | {ok, Pid, State} |  {error, Reason}
%% @doc
%%  'Type' is usually 'normal' but could indicated a takeover or failover,
%%  if this app is in a cluster. The StartArgs come from the pranayama.app
%%  file in the 'mod' section -  the list
%%  after 'pranayama_app' will be sent in as this param; currently
%%  there is only one such arg: the location of the config file.
%%
%%  <i>"This function is called whenever an application 
%%  is started using application:start/1,2, and should start the processes
%%  of the application. If the application is structured according to the
%%  OTP design principles as a supervision tree, this means starting the
%%  top supervisor of the tree."</i>
%%
%% @end
%%--------------------------------------------------------------------
start(_Type, [ConfigFile]) when is_list(ConfigFile) ->

    AppConfig = read_app_config(ConfigFile),
    case pranayama_sup:start_link(AppConfig) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @spec stop(State::any()) -> void()
%% @doc
%% Currently we have no clean-up to do at this level.
%%
%%  <i>"This function is called whenever an application
%%  has stopped. It is intended to be the opposite of Module:start/2 and
%%  should do any necessary cleaning up. The return value is ignored."</i> 
%%
%%
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    pranayama_logger:info("Pranayama is now stopping."),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec read_app_config(ConfigFile::string()) -> app_config()
%% @doc
%%
%%  We get all our config values from the pranayama.conf file, The
%%  #app_config record itself has no defaults. If any of these values
%%  are missing, we exit.
%%
%% @end
%%--------------------------------------------------------------------
read_app_config(ConfigFile) when is_list(ConfigFile)->

    Config = get_config(ConfigFile),
    
    #app_config{port_num=get_config_value(Config,port_num),
		web_app_registration_process_name=get_config_value(Config,web_app_registration_process_name),
		receive_request_timeout_ms=get_config_value(Config,receive_request_timeout_ms),
		handle_request_timeout_ms=get_config_value(Config,handle_request_timeout_ms),
		log_directory =get_config_value(Config,log_directory)
	       }.

%%--------------------------------------------------------------------
%% @spec get_config(ConfigFile::string()) -> any() 
%% @doc  
%% 
%% The contents of the ConfigFile should be a list of tuples,
%% each tuple representing a config Key and Value; this 
%% configuration is mandatory so we exit if we fail to read
%% from the file.
%%
%% @end
%%--------------------------------------------------------------------
get_config(ConfigFile) when is_list(ConfigFile)->

    case file:consult(ConfigFile) of
	{ok,[Config]} when is_list(Config)->
	    Config;
	{ok,InvalidConfig}->
	    exit({invalid_config,{ConfigFile,InvalidConfig}});
	Error ->
	    exit({failed_to_open_config_file,{ConfigFile,Error}})
    end.


%%--------------------------------------------------------------------
%% @spec get_config_value(Config::[tuple()],Key::atom()) -> any() 
%% @doc  
%% 
%% We assume here that all values are mandatory; if we fail
%% to find a value for the Key, we exit.
%%
%% @end
%%--------------------------------------------------------------------
get_config_value(Config,Key)
  when is_list(Config),
       is_atom(Key)->

    case lists:keysearch(Key,1,Config) of
	 
       {value,{Key,Value}}->
	    Value;
	
       false->
	    exit({failed_to_get_config_value,{Key,Config}})
	    
    end.

