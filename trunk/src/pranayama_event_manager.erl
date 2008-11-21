%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%  We currently have only one Event Manager.
%%%  
%%%  Please note that we add Event Handlers - like <em>pranayama_log</em>, for
%%%  instance - using gen_event:add_sup_handler/3 not
%%%  gen_event:add_handler/2; we do so that the
%%%  pranayama_event_manager_sup can be aware of the failure of any
%%%  registered Event Managers; this way, we can re-register them. It
%%%  is important to understand that <em>only the process that called
%%%  add_sup_handler</em> will be notified of these failures.  As
%%%  such, all registrations must be done via
%%%  pranayama_event_manager_sup.
%%%
%%%  Although this approach works, it feels quite clumsy, so we're
%%%  open to suggested improvements.
%%%  
%%% ==Licence==
%%% This program is free software; you can redistribute it and/or
%%%  modify it under the terms of the GNU General Public License as
%%%  published by the Free Software Foundation; either version 2 of
%%%  the License, or (at your option) any later version.
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
-module(pranayama_event_manager).

-export([start_link/0,add_sup_handler/2,notify/1]).

-export([log/1]).

-define(SERVER,?MODULE).


-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-define(TEST_MODE,true).
-else.
-define(TEST_MODE,false).
-endif.


%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | {error,{already_started,Pid}}
%% @doc
%%
%%  <i>"Creates an event manager process as part of a supervision
%%  tree. The function should be called, directly or indirectly, by
%%  the supervisor. It will, among other things, ensure that the event
%%  manager is linked to the supervisor. "</i>
%%
%% @end
%%--------------------------------------------------------------------
start_link()->
    gen_event:start_link({local,?SERVER}).

%%--------------------------------------------------------------------
%% @spec add_sup_handler(Module::atom(),Args::list()) -> ok | {'EXIT',Reason} | term() 
%% @doc
%%
%%  <i>"Adds a new event handler in the same way as add_handler/3 but
%%  will also supervise the connection between the event handler and
%%  the calling process. "</i>
%%
%% @end
%%--------------------------------------------------------------------
add_sup_handler(Module,Args)
  when is_atom(Module),
       is_list(Args)->

    case gen_event:add_sup_handler(?SERVER,Module,Args) of
	ok->
	    pranayama_logger:info("Added Event Handler:~p with Args:~p.~n",[Module,Args]),
	    ok;
	Response->
	    Response
    end.

%%--------------------------------------------------------------------
%% @spec notify(Event::term()) -> ok 
%% @doc
%% Asynchronously sends the Event message to all subscribed Event
%% Handlers. You'll note that we bypass the actual call if we're
%% in test mode as we wouldn't have a deployed event manager.
%% 
%%  <i>"Sends an event notification to the event manager EventMgrRef
%%  (?SERVER). The event manager will call Module:handle_event/2 for
%%  each installed event handler to handle the event."</i>
%%
%% @end
%%--------------------------------------------------------------------
notify(Event)->

    case ?TEST_MODE of
	false->
	    gen_event:notify(?SERVER,Event);
	true ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @spec log(LogMessage::log_message()) -> ok 
%% @doc
%% Convenient function which wraps up the LogMessage
%% into a notify call.
%% 
%%
%% @end
%%--------------------------------------------------------------------
log(LogMessage) when is_record(LogMessage,log_message)->    
    notify({log,LogMessage}).



    
    

