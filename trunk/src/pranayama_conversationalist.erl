% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%
%%%    Manages the concurrent http 'conversations'; a Conversation is
%%%    a single process of the pranayama_conversation module which
%%%    generates an http response for an http request.
%%%
%%%    While we can have multiple concurrent conversations
%%%    taking place, we can only have a single *offer*
%%%    of a new conversation; once this offer is accepted,
%%%    we can immediately offer a new connection; clearly,
%%%    the assurance that we are always offering a new conversation
%%%    is crucial, otherwise our http server is no longer functional.
%%%
%%%    There is a catch however: the Conversation must inform us
%%%    as soon as they have accepted  a client
%%%    connection.
%%%
%%%    As it is essential, we also take various opportunities
%%%    to ensure that we have this single offered connection.
%%% 
%%%    Initially we attempt to take over the desired port which
%%%    we use to listen for client connections, resulting in our
%%%    'listening socket'.
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
%%% @todo: Could add some unit tests here.
%%%-------------------------------------------------------------------
-module(pranayama_conversationalist).

-behaviour(gen_server).


-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/pranayama_internal.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @type state() = #state{app_config=app_config(),
%%                        listening_socket=socket(),
%%                        offered_conversation_pid=pid()
%%                       }.
%%--------------------------------------------------------------------
-record(state, {app_config,
		listening_socket,
		offered_conversation_pid
	       }).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start_link(AppConfig::app_config())
%%   -> {ok, pid()} | ignore | {error, Error::term()}
%% @doc
%%
%%  <i>"Starts the server"</i>
%%
%% @end
%%--------------------------------------------------------------------
start_link(AppConfig) when is_record(AppConfig,app_config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AppConfig], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init([AppConfig::app_config()]) -> {ok, State::state()}
%% @doc
%%
%%  We create our Listening Socket, of which we only ever have one;
%%  with this Socket we can offer our first Conversation via
%%  a spawned <em>pranayama_conversation</em>; it must be spawned since
%%  it is a blocking call (otherwise we'd hang in init until it the
%%  offered connection was accepted).
%%
%%  Note that we store the pid of our currently offered Conversation
%%  in the State; you'll see this essential bit used elsewhere.
%%
%%  Once we've initialised, we wait for our first Conversation to
%%  signal that they've starting to talk with a client, thus allowing
%%  us to offer the next Conversation and so forth ad neteum.
%%
%% @end
%%--------------------------------------------------------------------
init([AppConfig]) when is_record(AppConfig,app_config) ->

    ListeningSocket = open_listening_socket(AppConfig#app_config.port_num),
    
	
    OfferedConversationPid = offer_conversation(AppConfig,ListeningSocket),
    pranayama_logger:info("We've just offered our first Conversation (~p)~n",
			  [OfferedConversationPid]),

    {ok, #state{app_config=AppConfig,
		listening_socket=ListeningSocket,
		offered_conversation_pid=OfferedConversationPid
	       }}.

%%--------------------------------------------------------------------
%% @spec handle_call(_Request::term(), _From::from(), State::state())
%%           -> {reply,unexpected_call,State::state()}
%% @doc
%%  As we do not currently expect any calls, we say so.
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) when is_record(State,state) ->
    {reply, unexpected_call, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(_Request::term(),State::state())
%%           -> {noreply,State::state()}
%% @doc
%%  There is only one sort of cast we expect.
%%
%%  We expect our Conversations (<em>pranayama_conversation</em>) to notify us
%%  as soon as their offer has been accepted by a client. As only one
%%  Conversation can be on offer at one time, we only expect this
%%  message from our currently offered Conversation; if it isn't (which is
%%  odd), we simply take the opportunity to cautiously ensure that we
%%  have our single offered Conversation.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({offer_accepted,ConversationPid},State) 
  when is_pid(ConversationPid),
       is_record(State,state)->
    
    pranayama_logger:trace("A Conversation (~p) has reported that it has been accepted.",[ConversationPid]),
       
    case ConversationPid =:= State#state.offered_conversation_pid of
	
	true->
	
	    pranayama_logger:trace("It was our currently offered Conversation (~p) that has just accepted ... ~n",
				  [State#state.offered_conversation_pid]),
	    NewlyOfferedConversationPid = offer_conversation(State#state.app_config,
							   State#state.listening_socket),
	    pranayama_logger:trace("... so we have replaced it with newly created Conversation (~p).~n",[NewlyOfferedConversationPid]),
	    {noreply,State#state{offered_conversation_pid=NewlyOfferedConversationPid}};
	false->
	    
	    pranayama_logger:warn("A Conversation (~p) has just told us that it was accepted - but it's *not* our currently offered one (~p).",
				  [ConversationPid,State#state.offered_conversation_pid]),
	    {noreply,State#state{offered_conversation_pid=ensure_single_offered_conversation(State)}}
       
    end;

handle_cast({offer_accepted,undefined},State)
  when is_record(State,state)->

    pranayama_logger:warn("A Conversation (?) has just told us that it was accepted - but didn't provide a pid. Odd ..."),
	
    {noreply,State#state{offered_conversation_pid=ensure_single_offered_conversation(State)}};

handle_cast(_Msg, State)
  when is_record(State,state) ->

    pranayama_logger:warn("Some process just sent us a cast message (~p) but we weren't expected it"),    
    {noreply,State#state{offered_conversation_pid=ensure_single_offered_conversation(State)}}.


%%--------------------------------------------------------------------
%% @spec handle_info(_Request::term(),State::state())
%%           -> {noreply,State::state()}
%% @doc
%%  There is only one bit of info that interests us: when a 
%%  Conversation has exited.
%%
%%  Regardless of the reason for such an exit, we assert that a single
%%  Conversation is offering a connection; you'll note that even if it
%%  wasn't our currently offered Conversation that was terminated, we
%%  take the opportunity to make this assertion.
%%
%%  Please note that under normal conditions, upon the normal exit
%%  of any Conversation ('complete'), we should already have another
%%  Conversation on offer; however, if our currently offered Conversation
%%  crashes, then we certainly have to replace it.
%%
%%  Finally, unless the reporting process is our currently offered 
%%  Conversation, we cannot be sure that it was indeed a Conversation
%%  process; all we can do is to make our aforementioned sanity check.
%%
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT',ConversationPid,completed},State) 
  when is_pid(ConversationPid),
       is_record(State,state)->
    
    pranayama_logger:trace("A Conversation (~p) has completed successfully.~n",[ConversationPid]),
    {noreply,State#state{offered_conversation_pid=ensure_single_offered_conversation(State)}};

handle_info({'EXIT',ConversationPid,Reason},State) 
  when is_pid(ConversationPid),
       is_record(State,state)->

    case ConversationPid =:= State#state.offered_conversation_pid of
	
	true->
	    pranayama_logger:error("Our currently offered Conversation has been terminated because ~p.~n",[Reason]),
	    {noreply,State#state{
			     offered_conversation_pid=offer_conversation(State#state.app_config,State#state.listening_socket)}};
	false->
	    pranayama_logger:warn("A linked process (~p) - which was not "
				   "our currently offered Conversation was terminated, reason (~p),~n",
				   [ConversationPid,Reason]),

	    {noreply,State#state{offered_conversation_pid=ensure_single_offered_conversation(State)}}
	     
     end;
	    
handle_info(_Info, State) when is_record(State,state)->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec terminate(_Reason::term(), State::state())
%%           -> ok
%% @doc
%% As we've trapped the exits of all of our Conversations, they should 
%% automatically die along with this process.
%%
%% Apparently, as this process is the controlling process of the
%% listening socket (because it started it), the socket would be
%% closed automatically - but I'd rather make certain.
%%
%% Naturally, this module is in the supervision tree, so -  upon
%% an unexpected exit - we should have another process initialised.
%%
%% <i>"This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason.  The return value is ignored."</i>
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->

    pranayama_logger:warn("Conversationalist is about to be terminated (~p) ~n",[_Reason]),
    gen_tcp:close(State#state.listening_socket),
    pranayama_logger:info("We just closed our listening socket before the Conversationalist ~p is terminated~n",[self()]),
    ok.


%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState::state()}
%%
%% @doc
%% Currently unused. I suppose this is so we can maintain or
%% even convert our current State into a new State structure that
%% the updated code requires; something to do with release handling.
%%
%% <i>"Convert process state when code is changed"</i>
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) when is_record(State,state)->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec open_listening_socket(PortNum::integer())
%%   -> ListeningSocket::socket() 
%%
%% @doc
%% You'll notice that we're going with the {packet,http}
%% approach; please see the commentary for
%% <em>pranayama_request_reader</em> for details.  We also use a
%% passive socket - while this opens us up to abusively dominating
%% clients ... we'll maybe we should go 'active', in fact.. Finally,
%% 'reuseaddr' means a faster recovery if drop our control of the
%% port.
%%
%% Needless to say, if we cannot capture the desired port, we have no
%% choice but to exit.
%%
%% @end
%% @todo Switch to an active Listening Socket?
%%--------------------------------------------------------------------
open_listening_socket(PortNum) when is_integer(PortNum)->
    
    pranayama_logger:info("Attempting to open up a Listening Socket on PortNum ~p~n",[PortNum]),
    case gen_tcp:listen(PortNum,[binary,
				 {packet,http},
				 {active,false},
				 {reuseaddr,true}
				]) of
	{ok,ListeningSocket}->

	    {ok,{Address,PortNum}} = inet:sockname(ListeningSocket),
	    pranayama_logger:info("ListeningSocket (~p:~p) successfully opened.~n",[Address,PortNum]),

	    ListeningSocket;
	
        {error,eaddrinuse}->
	    exit("Failed to get a Listening Socket as the port ~p is already in use.",[PortNum])
	    
    end.


%%--------------------------------------------------------------------
%% @spec offer_conversation(AppConfig::app_config(),
%%                          ListeningSocket::socket())
%%                         -> pid()
%%
%% @doc
%%  A simple wrapper to spawn a Conversation which will immediately
%%  offer a connection to potential clients.
%%
%% @end
%% @todo Isn't that process_flag redundant beside spawn_link?
%%--------------------------------------------------------------------
offer_conversation(AppConfig,ListeningSocket) when is_record(AppConfig,app_config)->
    process_flag(trap_exit,true),
    spawn_link(fun() -> pranayama_conversation:start(AppConfig,ListeningSocket) end).

    

%%--------------------------------------------------------------------
%% @spec ensure_single_offered_conversation(State::state())-> pid()
%%
%% @doc
%% As mentioned above, our approach is to offer only one
%% Conversation at a time. Apparently, recent versions of Erlang allow
%% multiple connections to be offered thus allowing pooling; perhaps
%% there would be a performance advantage to doing this but for the
%% present, it's working fairly well; I also have the impression that
%% connections are quick to be offered.
%%
%% We pass along the State#state.offered_conversation_pid to
%% ensure_single_offered_conversation/2.
%%
%% @end
%%--------------------------------------------------------------------
ensure_single_offered_conversation(State) when is_record(State,state)->
    ensure_single_offered_conversation(State,State#state.offered_conversation_pid).

%%--------------------------------------------------------------------
%% @spec ensure_single_offered_conversation(State::state(),pid())
%%                 -> pid()
%%
%% @doc
%% We ensure that the supplied process is actually alive; if not
%% we offer a new Conversation as the missing one was our currently offered;
%% it would be nice to ensure that the Pid - if alive - does actually
%% represent a Conversation; I don't know how to do this.
%% 
%% @end
%%--------------------------------------------------------------------
ensure_single_offered_conversation(State,undefined)->

    pranayama_logger:error("Somehow we are without an offered Conversation ..."),
    NewOfferedConversationPid = offer_conversation(State#state.app_config,State#state.listening_socket),
    pranayama_logger:error(".. however, we just replaced it with a new one ~p",[NewOfferedConversationPid]),
    NewOfferedConversationPid;

ensure_single_offered_conversation(State,OfferedConversationPid)
  when is_record(State,state),
       is_pid(OfferedConversationPid)->
    
    case process_info(OfferedConversationPid)=/=undefined of
	true->
	   OfferedConversationPid;
	false->
	    pranayama_logger:error("Although we did have a Pid for the offered Conversation (~p),"
				   "the actual process doesn't exist ...~n",
				   [OfferedConversationPid]),
	    NewlyOfferedConversationPid = offer_conversation(State#state.app_config,State#state.listening_socket),
	    pranayama_logger:info(".. so we'll replace it with a new one ~p.~n",
				  [NewlyOfferedConversationPid]),
	    NewlyOfferedConversationPid
	    
    end.
				   
