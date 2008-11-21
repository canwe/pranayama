%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%  The primary purpose of this module is to maintain
%%%  the list of 'registered' webapps; these webapps
%%%  register themselves by calling this globally registered 
%%%  process; the Registrar also informs these webapps
%%%  when it has been restarted, so that they have 
%%%  a chance to re-registered themselves; unfortunately,
%%%  if this process dies, its state cannot be preserved.
%%%
%%%  All webapps must be globally registered processes.
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
-module(pranayama_registrar).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% @type state() = #state{app_config=app_config(),
%%                        registered_web_apps=[web_app_config()]
%%                       }.
%%--------------------------------------------------------------------
-record(state, {app_config,
		registered_web_apps=[]
	       }).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start_link(AppConfig::app_config()) ->
%%         {ok, pid()} | ignore | {error, Error::term()}
%% @doc
%%  Please note that we register a global name, as the webapps
%%  are presumed to be on different nodes than the one that
%%  is running Pranayama.
%% @end
%%--------------------------------------------------------------------
start_link(AppConfig) when is_record(AppConfig,app_config) ->
    gen_server:start_link({global,
			   AppConfig#app_config.web_app_registration_process_name},
			  ?MODULE, [AppConfig], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(_Args::[AppConfig::app_config()]) -> {ok, State::state()}
%% @doc
%%  We currently have no initialisation to do.
%%
%% @end
%%--------------------------------------------------------------------
init([AppConfig]) when is_record(AppConfig,app_config) ->
    pranayama_logger:info("Starting the Registrar ..."),
    {ok, #state{app_config=AppConfig, registered_web_apps=[]}}.

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
%% There are several calls that we currently recognize:
%% <ul>
%% <li>Registration of a web app.</li>
%% <li>Polling to see if a web app is actually registered.</li>
%% <li>Removing a web app from registration.</li>
%% <li>Retrieving the global process name that belongs to the provided alias.</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
handle_call({register,WebAppConfig}, _From, State)
  when is_record(WebAppConfig,web_app_config) ->

    case register_web_app(State#state.registered_web_apps,WebAppConfig) of
	RegisteredWebApps when is_list(RegisteredWebApps)->

	    pranayama_logger:info("Registered a Web App:~p",[WebAppConfig]),

	    {reply,{register_response,ok},State#state{registered_web_apps=RegisteredWebApps}};
	
	{error,alias_already_registered}=Response ->
	    {reply,Response,State}
    end;

handle_call({is_registered,WebAppConfig}, _From, State) when is_record(WebAppConfig,web_app_config) ->

    Verdict =  is_web_app_registered(State#state.registered_web_apps,WebAppConfig),
    {reply,{is_registered_response,Verdict},State};

handle_call({deregister,WebAppConfig}, _From, State) when is_record(WebAppConfig,web_app_config) ->

    RegisteredWebApps = deregister_web_app(State#state.registered_web_apps,WebAppConfig),
    {reply,{deregister_response,ok},State#state{registered_web_apps=RegisteredWebApps}};

handle_call({get_web_app_global_name,WebAppAlias}, _From, State)->

    {reply, 
     {get_web_app_global_name_response,get_web_app_global_name(State#state.registered_web_apps,WebAppAlias)},
     State};

handle_call(_Request, _From, State) ->
    pranayama_logger:trace("Unhandled call:~p",[_Request]),
    {reply, unhandled_call, State}.



%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State::state()}
%% @doc
%%
%% We expect no casts; however, if we are called we simply log it.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    pranayama_logger:trace("Unhandled cast:~p",[_Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State::state} 
%% @doc
%% We expect no info messages; however, if we are called we simply log it.
%%
%% @end
%% @todo: Why can't I have a @spec for each function clause?
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    pranayama_logger:trace("Unhandled info:~p",[_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec terminate(_Reason::term(), _State::state()) -> void()
%% @doc
%% The only pre-termination action we take is to inform all of
%% our registered webapps that we are about to terminate, thus
%% giving them a fighting chance to re-register themselves.
%% 
%%  <i>"This function is called by a gen_server when it is about to
%%  terminate. It should be the opposite of Module:init/1 and do any
%%  necessary cleaning up. When it returns, the gen_server terminates
%%  with Reason. The return value is ignored."</i>
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    pranayama_logger:error("Terminiating the Registrar  so informing all registered apps"),
    inform_all_apps_of_termination(State#state.registered_web_apps),
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
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec inform_all_apps_of_termination(RegisteredWebApps::[web_app_config()])
%%                             -> ok
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
inform_all_apps_of_termination(RegisteredWebApps)
  when is_list(RegisteredWebApps)->
    lists:map(fun(RegisteredWebApp)->
		      inform_app_of_termination(RegisteredWebApp)
	      end,
	      RegisteredWebApps),
    ok.

%%--------------------------------------------------------------------
%% @spec inform_app_of_termination(RegisteredWebApps::web_app_config())
%%                             -> ok
%% @doc
%%  We send a cast as we're not interested in their response (even if
%%  they could send one ... could they, during termination?).
%%
%% @end
%%--------------------------------------------------------------------
inform_app_of_termination(RegisteredWebApp) when is_record(RegisteredWebApp,web_app_config)->

    case gen_server:cast({global,RegisteredWebApp#web_app_config.global_process_name},pranayama_terminating) of

	ok->
	    pranayama_logger:error("Informed app ~p of termination.",[RegisteredWebApp]),
	    ok;
	Other ->
	    pranayama_logger:error("Error (~p) when informating app ~p of termination.",[Other,RegisteredWebApp]),
	    ok
    end.
    


%%--------------------------------------------------------------------
%% @spec register_web_app(RegisteredWebApps::[web_app_config()],
%%                        WebAppConfig::web_app_config())
%%                             -> ok
%% @doc
%%  Once you add a web app config for a particular alias, you cannot
%%  add another for the same alias; you would have to deregister it first.
%%  It's a small form of protection. The uniqueness is case-insensitive.
%%
%% @end
%%--------------------------------------------------------------------
register_web_app(RegisteredWebApps,WebAppConfig) 
  when is_list(RegisteredWebApps),
       is_record(WebAppConfig,web_app_config)->

    case is_web_app_alias_registered(RegisteredWebApps,WebAppConfig) of
	true->
	   {error,alias_already_registered};
	false->
	    [WebAppConfig|RegisteredWebApps]
    end.

%%--------------------------------------------------------------------
%% @spec is_web_app_registered(RegisteredWebApps::[web_app_config()],
%%                        TargetWebAppConfig::web_app_config())
%%                             -> boolean()
%% @doc
%% Note that we're looking for the exact match - not merely the alias.
%%
%% @end
%%--------------------------------------------------------------------
is_web_app_registered(RegisteredWebApps,TargetWebAppConfig) 
  when is_list(RegisteredWebApps),
       is_record(TargetWebAppConfig,web_app_config)->

    lists:member(TargetWebAppConfig,RegisteredWebApps).


%%--------------------------------------------------------------------
%% @spec is_web_app_alias_registered(RegisteredWebApps::[web_app_config()],
%%                        TargetWebAppConfig::web_app_config())
%%                             -> boolean()
%% @doc
%%  But in this case, we are just looking for an alias match - with some
%%  degree of tolerance.
%% @end
%%--------------------------------------------------------------------
is_web_app_alias_registered(RegisteredWebApps,TargetWebAppConfig) 
  when is_list(RegisteredWebApps),
       is_record(TargetWebAppConfig,web_app_config)->

    WebAppConfigs = [WebAppConfig || WebAppConfig <- RegisteredWebApps,
				     pranayama_utils:prepare_for_compare(WebAppConfig#web_app_config.alias)
					 =:= pranayama_utils:prepare_for_compare(TargetWebAppConfig#web_app_config.alias)],

    WebAppConfigs=/=[].


%%--------------------------------------------------------------------
%% @spec deregister_web_app(RegisteredWebApps::[web_app_config()],
%%                        TargetWebAppConfig::web_app_config())
%%                             -> [web_app_config()]
%% @doc
%% We return the new list with the target filtered out.
%% @end
%%--------------------------------------------------------------------
deregister_web_app(RegisteredWebApps,TargetWebAppConfig) 
  when is_list(RegisteredWebApps),
       is_record(TargetWebAppConfig,web_app_config)->

    [WebAppConfig || WebAppConfig <- RegisteredWebApps,
				     WebAppConfig=/=TargetWebAppConfig].


%%--------------------------------------------------------------------
%% @spec get_web_app_global_name(RegisteredWebApps::[web_app_config()],
%%                        TargetWebAppConfig::web_app_config())
%%                             -> string() | undefined
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_web_app_global_name(RegisteredWebApps,TargetWebAppAlias) 
  when is_list(RegisteredWebApps),
       is_atom(TargetWebAppAlias)->
    get_web_app_global_name(RegisteredWebApps,atom_to_list(TargetWebAppAlias));

get_web_app_global_name(RegisteredWebApps,TargetWebAppAlias) 
  when is_list(RegisteredWebApps),
       is_list(TargetWebAppAlias)->
    
    WebAppConfigs = [WebAppConfig || WebAppConfig <- RegisteredWebApps,
				     pranayama_utils:prepare_for_compare(WebAppConfig#web_app_config.alias)
					 =:=pranayama_utils:prepare_for_compare(TargetWebAppAlias)],
    
    case WebAppConfigs=/=[] of
	true->
	    [WebAppConfig|_] = WebAppConfigs,
	    WebAppConfig#web_app_config.global_process_name;
	false ->
	    undefined
    end.



%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
%% @private
get_web_app_global_name_test()->

    ?assertEqual(apple,get_web_app_global_name(generate_test_registrations(),
					       "apple")),
    
    ?assertEqual(apple,get_web_app_global_name(generate_test_registrations(),
					       "Apple")),
    
    ?assertEqual(apple,get_web_app_global_name(generate_test_registrations(),
					       " ApPle ")),

    ?assertEqual(banana,get_web_app_global_name(generate_test_registrations(),
					       banana)),

    ?assertEqual(undefined,get_web_app_global_name(generate_test_registrations(),
					       "grape")),

    
    ok.

%% @private
handle_call_register_test()->

    OriginalApple = generate_web_app_config("apple",apple),
    ExistingWebApps = [OriginalApple],

    Pineapple = generate_web_app_config("sweet_pineapple",pineapple),

    ?assertMatch({reply,{register_response,ok},#state{registered_web_apps=[Pineapple|ExistingWebApps]}},
		 handle_call({register,Pineapple},undefined,#state{registered_web_apps=ExistingWebApps})),

    ?assertMatch({reply,{error,alias_already_registered},#state{registered_web_apps=[Pineapple|ExistingWebApps]}},
		 handle_call({register,Pineapple},undefined,#state{registered_web_apps=[Pineapple|ExistingWebApps]})),

    
    
    ok.

%% @private
handle_call_get_web_app_global_name_test()->

    ExistingWebApps = generate_test_registrations(),

    State = #state{registered_web_apps=ExistingWebApps},

    ?assertMatch({reply,{get_web_app_global_name_response,apple},State},
		 handle_call({get_web_app_global_name,"apple"},undefined,State)),

    ?assertMatch({reply,{get_web_app_global_name_response,apple},State},
		 handle_call({get_web_app_global_name," APPle "},undefined,State)),


    ?assertMatch({reply,{get_web_app_global_name_response,undefined},State},
		 handle_call({get_web_app_global_name,"miracle"},undefined,State)),

    
    ok.

%% @private
terminate_test()->
    
    ExistingWebApps = generate_test_registrations(),

    State = #state{registered_web_apps=ExistingWebApps},

    ?assertMatch(ok,terminate(undefined,State)),

    ok.

%% @private
handle_call_catchall_test()->
    
    ExistingWebApps = generate_test_registrations(),

    State = #state{registered_web_apps=ExistingWebApps},

    ?assertMatch({reply,unhandled_call,State},
		 handle_call(chelsea_hotel,undefined,State)),

    ok.

%% @private
handle_call_deregister_test()->

    Apple = generate_web_app_config("apple",apple),
    Banana = generate_web_app_config("banana",banana),
    BananaCopy = generate_web_app_config("banana",banana),

    ?assertMatch({reply,{deregister_response,ok},#state{registered_web_apps=[Banana]}},
		 handle_call({deregister,Apple},undefined,#state{registered_web_apps=[Apple,Banana]})),
    
    ?assertMatch({reply,{deregister_response,ok},#state{registered_web_apps=[]}},
		 handle_call({deregister,Banana},undefined,#state{registered_web_apps=[Banana]})),

    ?assertMatch({reply,{deregister_response,ok},#state{registered_web_apps=[]}},
		 handle_call({deregister,BananaCopy},undefined,#state{registered_web_apps=[Banana]})),

    
    ?assertMatch({reply,{deregister_response,ok},#state{registered_web_apps=[]}},
		 handle_call({deregister,Banana},undefined,#state{registered_web_apps=[]})),

    
    ok.

%% @private
handle_call_is_registered_test()->

    Apple = generate_web_app_config("apple",apple),
    ExistingWebApps = [Apple],

    Pineapple = generate_web_app_config("sweet_pineapple",pineapple),

    ?assertMatch({reply,{is_registered_response,true},#state{registered_web_apps=ExistingWebApps}},
		 handle_call({is_registered,Apple},undefined,#state{registered_web_apps=ExistingWebApps})),

    ?assertMatch({reply,{is_registered_response,false},#state{registered_web_apps=ExistingWebApps}},
		 handle_call({is_registered,Pineapple},undefined,#state{registered_web_apps=ExistingWebApps})),

    
    ok.


%% @private
generate_test_registrations()->
    [generate_web_app_config("apple",apple),
     generate_web_app_config("banana",banana),
     generate_web_app_config("carrot",carrot)].
     

%% @private
generate_web_app_config(Alias,GlobalProcessName)
  when is_list(Alias),
       is_atom(GlobalProcessName)->

    #web_app_config{alias=Alias,global_process_name=GlobalProcessName}.
     
     
     
			       
