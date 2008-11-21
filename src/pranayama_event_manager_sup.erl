%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%  This is for ensuring that all of our Event Handlers
%%%  remain registered with <em>pranayama_event_manager</em>; however
%%%  this module is not a 'supervisor' in the OTP sense.
%%%
%%%  Please refer to comments made in <em>pranayama_event_manager</em>.
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
-module(pranayama_event_manager_sup).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER, ?MODULE). 

-include("../include/pranayama_internal.hrl").

%% @type state() = #state{app_config=app_config()}. 
-record(state, {app_config}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec start_link(AppConfig::app_config()) -> {ok, pid()} | ignore | {error, Error::term()}
%% @doc
%%
%%  <i>"Starts the server"</i>
%%
%% @end
%%--------------------------------------------------------------------
start_link(AppConfig)
  when is_record(AppConfig,app_config) ->

    case gen_server:start_link({local, ?SERVER}, ?MODULE, [AppConfig], []) of
	{ok,Pid}->
	    pranayama_logger:info("Event Manager Supervisor started..."),
	    {ok,Pid};
	Response ->
	    Response
    end.
	    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec init(_Args::[AppConfig::app_config()]) -> {ok, State::state()}
%% @doc
%%
%%  Called after start_link(), if successful. This is where we
%%  registered all of the Handlers; as mentioned above, it is
%%  the only place that they can be registered, if we want
%%  to keep them registered in the event of handler failure.
%%
%% @end
%%--------------------------------------------------------------------
init([AppConfig]=_Args) when is_record(AppConfig,app_config) ->

    pranayama_event_manager:add_sup_handler(pranayama_log_writer,[AppConfig]),
    pranayama_event_manager:add_sup_handler(pranayama_log,[AppConfig]),

    pranayama_logger:trace("All Event Handlers have been added ..."),

    {ok, #state{app_config=AppConfig}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request::term(), _From::from(), State::state()) ->
%%                                   {reply, Reply::term(), State::state()} |
%%                                   {reply, Reply::term(), State::state(), Timeout::timeout()} |
%%                                   {noreply, State::state()} |
%%                                   {noreply, State::state(), Timeout::timeout()} |
%%                                   {stop, Reason::term(), Reply::term(), State::state()} |
%%                                   {stop, Reason::term(), State::state()}
%% @doc
%%
%% We expect no calls; however, if we are called we simply log it
%% and return 'ok'.
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    
    pranayama_logger:warn("Event Manager Supervisor received"
			  " and unexpected call: ~p",
			  [Request]),

    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State::state()} 
%% @doc
%%
%% We expect no casts; however, if we are called we simply log it.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->

    pranayama_logger:warn("Event Manager Supervisor received"
			  " and unexpected cast: ~p",
			  [Msg]),



    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State::state} |
%%                                   {noreply, State::state, Timeout::timeout()} |
%%                                   {stop, Reason::term(), State::state()}
%% @doc
%%  We are expecting only one type of info message: that which
%%  tells us that one of our registered Handlers has failed, and
%%  was therefore deregistered; as such, we simply reregister it.
%%  Otherwise, we simply log any unexpected messages.
%%
%% @end
%% @todo: Why can't I have a @spec for each function clause?
%%--------------------------------------------------------------------
handle_info({gen_event_EXIT, HandlerModule, Reason}, State)
  when is_atom(HandlerModule)->

    pranayama_logger:info("An Event Handler ~p was deregistered from"
			   "our Event Manager, so we'll re-register it. Reason was:~p",
			   [HandlerModule,Reason]),
    pranayama_event_manager:add_sup_handler(HandlerModule,[State#state.app_config]),
    {noreply,State};

handle_info(_Info, State) ->

    pranayama_logger:warn("Event Manager Supervisor received"
			  " and an unexpected info message : ~p",
			  [_Info]),
   
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec terminate(_Reason::term(), _State::state()) -> void()
%% @doc
%%
%%  <i>"This function is called by a gen_server when it is about to
%%  terminate. It should be the opposite of Module:init/1 and do any
%%  necessary cleaning up. When it returns, the gen_server terminates
%%  with Reason. The return value is ignored."</i>
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    pranayama_logger:warn("Terminating Event Manager Supervisor:~p",[_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc
%%
%%  <i>"Convert process state when code is changed"</i>
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    pranayama_logger:info("Changing Event Manager Supervisor"),
    {ok, State}.

