%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% Various utility functions useful to several modules; mostly
%%% related to #http_request and #http_response.
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
%%% @todo Document.
%%%-------------------------------------------------------------------
-module(pranayama_utils).


-export([fallback/2,
	 figure_client_description/1,
	 figure_content_length/1,
	 figure_normalized_params/1,
	 figure_request_method/1,
	 figure_response_format/1,
	 figure_web_app_alias/1,
	 figure_web_app_path/1,
	 get_param_value/2,
         get_param_values/2,
	 pad/2,
	 prepare_for_compare/1,
	 search_for_tuple_value/3
	]).

-include("../include/pranayama.hrl").
-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

fallback(undefined,FallbackValue)->
    FallbackValue;
fallback(Whatever,_FallbackValue) ->
    Whatever.

%%--------------------------------------------------------------------
%% @spec figure_client_description(http_request()) -> string()
%% @doc Otherwise known as the 'User-Agent', for example:
%% Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.4; en-US; rv:1.9.0.3) Gecko/2008092414 Firefox/3.0.3
%% @todo Test
%%--------------------------------------------------------------------
figure_client_description(HttpRequest)->
    extract_header_value(HttpRequest#http_request.headers,?CLIENT_DESC_HEADER_KEY).
    
%%--------------------------------------------------------------------
%% @spec figure_content_length(Headers::headers()) -> integer()
%% @doc Returns 0 (zero) as a fallback.
%%--------------------------------------------------------------------
figure_content_length(Headers)
  when is_list(Headers)->
    
    HeaderValue = extract_header_value(Headers,"Content-Length"),
    case HeaderValue=/=undefined of
	true->
	  list_to_integer(HeaderValue);
	false->
	  0
    end.

%%--------------------------------------------------------------------
%% @spec figure_normalized_params(http_request()) -> params()
%% @doc By 'normalize' we mean that any duplicate keys are
%% folder into a single key, with a list of values; even 
%% for keys of single values, we use a list of a single value,
%% for consistency.
%% @end
%%--------------------------------------------------------------------
figure_normalized_params(HttpRequest)
  when is_record(HttpRequest,http_request)->
    normalize_params(figure_params(HttpRequest)).


%%--------------------------------------------------------------------
%% @spec figure_request_method(string()) -> atom()
%% @doc We attempt to match to one of our recognized method types
%% (REQUEST_METHODS); returns FALLBACK_REQUEST_METHOD as a fallback.
%% @end
%%--------------------------------------------------------------------
figure_request_method(undefined) ->

    ?FALLBACK_REQUEST_METHOD;

figure_request_method(HttpRequest) when is_record(HttpRequest,http_request) ->
    figure_request_method(HttpRequest#http_request.method);
figure_request_method(RawMethod) when is_atom(RawMethod)->
    figure_request_method(atom_to_list(RawMethod));
figure_request_method(RawMethod) when is_list(RawMethod)->


    case lists:keysearch(RawMethod,1,?REQUEST_METHODS) of
	 
       {value,{RawMethod,Method}}->
	    Method;
	
       _Otherwise->
	    ?FALLBACK_REQUEST_METHOD
	    
    end.


%%--------------------------------------------------------------------
%% @spec figure_response_format(http_request()) -> atom()
%% @doc
%% As part of the @link http_request(), the client can request that the
%% response be of a specific format; this is not to say that the
%% actual webapp will support it, of course. We attempt to match
%% the value of the ?FORMAT_PARAM_KEY param to one
%% of our recognized content types (?RESPONSE_FORMATS);
%% returns ?FALLBACK_RESPONSE_FORMAT as a fallback; if the
%% relevant param isn't present, use also use this fallback.
%% @end
%%--------------------------------------------------------------------
figure_response_format(HttpRequest)
  when is_record(HttpRequest,http_request)->
 
    figure_response_format(get_param_value(HttpRequest,?FORMAT_PARAM_KEY));

figure_response_format(undefined) ->
    ?FALLBACK_RESPONSE_FORMAT;
figure_response_format(RawFormat) when is_list(RawFormat) ->

    case lists:keysearch(RawFormat,1,?RESPONSE_FORMATS) of
	 
       {value,{RawFormat,Format}}->
	    Format;
	
       _Otherwise->
	    ?FALLBACK_RESPONSE_FORMAT
	    
    end.

%%--------------------------------------------------------------------
%% @spec figure_web_app_alias(http_request()) -> string() | undefined
%% @doc
%%  The alias is that part between the / following the host and
%%  the second /; for example, if we have a uri of http://www.everybodyknows.org/Gun/Whatever,
%%  the alias would be 'Whatever'
%% @end
%%--------------------------------------------------------------------
figure_web_app_alias(HttpRequest)
  when is_record(HttpRequest,http_request)->

    figure_web_app_alias(HttpRequest#http_request.uri);

%%--------------------------------------------------------------------
%% @todo Test again - Uri starts with /Gun/Whatever
%%--------------------------------------------------------------------
figure_web_app_alias(Uri) when is_list(Uri) ->
    case string:tokens(Uri,[$/]) of
	[Alias|_] ->
	    Alias;
	_ ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% @spec figure_web_app_path(http_request()) -> string() | undefined
%% @doc
%%  The web app path is that part between the / following the alias  and
%%  query portion; for example, if we have a uri of http://www.everybodyknows.org/Gun/Whatever/Who?querystuff
%%  the path would be "Whatever/Who"; you'll note that we don't return the leading slash.
%% @end
%%--------------------------------------------------------------------

figure_web_app_path(HttpRequest)
  when is_record(HttpRequest,http_request)->

    case string:tokens(HttpRequest#http_request.uri,[$?]) of

	[BeforeQuery, _Query]->
	    figure_web_app_path(BeforeQuery);
	[PathWithoutQuery]->
	    figure_web_app_path(PathWithoutQuery);
	_->    
	  undefined

    end;
figure_web_app_path(PathWithoutQuery)->
    case string:tokens(PathWithoutQuery,[$/]) of
		[_alias|PathParts]->
		    string:join(PathParts,"/");
		_->
		   undefined
	    end.


extract_header_value(Headers,Key)
  when is_list(Headers), is_list(Key)->

    NormalizedKey = normalize_key(Key),
    case lists:keysearch(NormalizedKey,1,normalize_keys(Headers)) of
	 
       {value,{NormalizedKey,Value}}->
	    Value;
	
       _Otherwise->
	    undefined
	    
    end.

normalize_keys(Headers) when is_list(Headers)->
    [{normalize_key(Key),Value} || {Key,Value} <- Headers].

normalize_key(Key) when is_list(Key)->
    string:to_upper(string:strip(Key)).



    
%%--------------------------------------------------------------------
%% Returns [{"key_a","value_1"},{"key_b","value_2",{"key_a","value_3"}]
%% Please note that the keys can repeat.
%%--------------------------------------------------------------------
figure_params(#http_request{method=get,_=_}=HttpRequest)
  when is_record(HttpRequest,http_request)->

    case string:tokens(HttpRequest#http_request.uri,[$?]) of
	[_BeforeQuery,Query]->
	    extract_params(Query);
	_Otherwise->
	    []
    end;
figure_params(HttpRequest)
  when is_record(HttpRequest,http_request)->
    extract_params(HttpRequest#http_request.body).

extract_params(undefined)->
    [];
extract_params(Cluster) when is_list(Cluster)->

    KeyValues = lists:map(
		  fun(KeyValuePair)->
		      case string:tokens(KeyValuePair,[$=]) of
			  [Key,Value]->
			      {Key,Value};
			  _ ->
			     undefined
		      end
		  end,
		  string:tokens(Cluster,[$&])),
    lists:filter(fun(KeyValue)->
			 KeyValue=/= undefined
		 end,
		 KeyValues).				  

			      


			     
			    

get_param_values(HttpRequest,Key)
  when is_record(HttpRequest,http_request), is_list(Key)->

    get_param_values(figure_normalized_params(HttpRequest),Key);

get_param_values(NormalizedParams,Key)
  when is_list(NormalizedParams), is_list(Key)->

    case lists:keysearch(Key,1,NormalizedParams) of
	 
       {value,{Key,Values}}->
	    Values;
       _Otherwise->
	    undefined
	    
    end.

get_param_value(NormalizedParams,Key)
  when is_list(NormalizedParams), is_list(Key)->

    case get_param_values(NormalizedParams,Key) of
	 
	[]->
	    undefined;
	
	Values when is_list(Values)->
	    [FirstValue|_] = Values,
	    FirstValue;
	
       _Otherwise->
	    undefined
	    
    end;
    
get_param_value(HttpRequest,Key)
  when is_record(HttpRequest,http_request), is_list(Key)->

    get_param_value(figure_normalized_params(HttpRequest),Key).
    

%%--------------------------------------------------------------------
%% @doc Results in a list of {Key,[Values]} elements such
%% that we are left without duplicate keys; even if a given
%% key has a single value, it is within a list. The actual
%% keys and values themselves are left exactly as we found
%% them. The keys are sorted alphabetically, but the values
%% for the same key are in the original order.
%% @end
%%--------------------------------------------------------------------
normalize_params(Params) when is_list(Params)->
    
     UniqueParamKeys =  lists:usort([Key||{Key,_Value}<-Params]),
     [make_param_element(Params,Key) || Key <- UniqueParamKeys].
  
make_param_element(Params,TargetKey) ->
    KeyValues = [Value || {Key,Value} <- Params, 
	      Key == TargetKey],
    {TargetKey,KeyValues}.


pad(String,Width) when is_list(String),is_integer(Width)->
    Template = lists:flatten(lists:concat(
			      ["~-",
			     Width,
			     "s"])),
    lists:flatten(io_lib:format(Template,[String])).


prepare_for_compare(Atom) when is_atom(Atom)->
    prepare_for_compare(atom_to_list(Atom));
prepare_for_compare(String) when is_list(String)->    
    ucase_and_trim(String).
    
ucase_and_trim(String) when is_list(String)->
    string:to_upper(string:strip(String)).


search_for_tuple_value(Key,Tuples,FallbackValue) when is_list(Tuples)->

    case lists:keysearch(Key,1,Tuples) of

	{value,{Key,TupleValue}} ->
	    TupleValue;
	
	false->
	    FallbackValue

    end.
    
%% Test Methods

%% @private
figure_content_length_test()->

    HttpRequest = generate_base_get_request(),

    ?assertEqual(1234,pranayama_utils:figure_content_length(HttpRequest#http_request.headers)),
    ?assertEqual(1234,pranayama_utils:figure_content_length([{"CONTENT-LenGth","1234"}])),
    ?assertEqual(1234,pranayama_utils:figure_content_length([{"   CONTENT-LenGth  ","1234"}])),
    ?assertEqual(0,pranayama_utils:figure_content_length([{"Content-len","1234"}])),
    
    ok.

%% @private
figure_normalized_params_test()->

    GetHttpRequest = generate_base_get_request(),
    ?assertMatch([{"country",["France","Czechoslovakia"]},
		  {"firstName",["Milan"]},{"lastName",["Kundera"]}
		 ],
		 pranayama_utils:figure_normalized_params(GetHttpRequest)),

    ?assertMatch([{"key",["value"]}],
		 pranayama_utils:figure_normalized_params(GetHttpRequest#http_request{uri="www/app?key=value&anotherkey="})),
    
    ?assertMatch([{"key",["value"]}],
		 pranayama_utils:figure_normalized_params(GetHttpRequest#http_request{uri="www/app?key=value&anotherkey"})),
    

    ?assertMatch([],
		 pranayama_utils:figure_normalized_params(GetHttpRequest#http_request{uri="www/app"})),

    PostHttpRequest = generate_base_post_request(),
    ?assertMatch([{"country",["France","Czechoslovakia"]},
		  {"firstName",["Milan"]},{"lastName",["Kundera"]}
		 ],
		 pranayama_utils:figure_normalized_params(PostHttpRequest)),

    
    ok.

%% @private
figure_response_format_test()->

    GetHttpRequest = generate_base_get_request(),

    ?assertEqual(html,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app?format=html"})),
    ?assertEqual(json,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app?format=json"})),
    ?assertEqual(text,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app?format=text"})),
    ?assertEqual(xml,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app?format=xml"})),
    ?assertEqual(html,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app?format=dsadasdadasda"})),
    ?assertEqual(html,pranayama_utils:figure_response_format(GetHttpRequest#http_request{uri="www/app"})),

    PostHttpRequest = generate_base_post_request(),
    ?assertEqual(xml,pranayama_utils:figure_response_format(PostHttpRequest#http_request{body="format=xml"})),
    ?assertEqual(html,pranayama_utils:figure_response_format(PostHttpRequest#http_request{body=undefined})),
    
    ok.

%% @private
figure_web_app_alias_test()->

    GetHttpRequest = generate_base_get_request(),
    ?assertEqual("TheAnswerMachine",pranayama_utils:figure_web_app_alias(GetHttpRequest)),
    ?assertEqual("TheAlias",pranayama_utils:figure_web_app_alias(GetHttpRequest#http_request{uri="TheAlias"})),
    ?assertEqual(undefined,pranayama_utils:figure_web_app_alias(GetHttpRequest#http_request{uri=""})),
       
		 
    ok.

%% @private
figure_web_app_path_test()->

    GetHttpRequest = generate_base_get_request(),
    ?assertEqual("arts/books",pranayama_utils:figure_web_app_path(GetHttpRequest)),
    

		 
    ok.

%% @private
figure_request_method_test()->

    ?assertEqual(delete,pranayama_utils:figure_request_method(#http_request{method="DELETE"})),
    ?assertEqual(get,pranayama_utils:figure_request_method(#http_request{method="GET"})),
    ?assertEqual(head,pranayama_utils:figure_request_method(#http_request{method="HEAD"})),
    ?assertEqual(post,pranayama_utils:figure_request_method(#http_request{method="POST"})),
    ?assertEqual(put,pranayama_utils:figure_request_method(#http_request{method="PUT"})),
    ?assertEqual(get,pranayama_utils:figure_request_method(#http_request{method="MADNESS"})),
    ?assertEqual(get,pranayama_utils:figure_request_method(#http_request{method=undefined})),
    
    ok.
		 
%% @private    
get_param_value_with_request_test()->

    GetHttpRequest = generate_base_get_request(),
    ?assertEqual("Kundera",pranayama_utils:get_param_value(GetHttpRequest,"lastName")),
    ?assertEqual("France",pranayama_utils:get_param_value(GetHttpRequest,"country")),

    ok.

%% @private
get_param_values_with_request_test()->

    GetHttpRequest = generate_base_get_request(),
    ?assertEqual(["Kundera"],pranayama_utils:get_param_values(GetHttpRequest,"lastName")),
    ?assertEqual(["France","Czechoslovakia"],pranayama_utils:get_param_values(GetHttpRequest,"country")),

    ok.

%% @private
generate_base_get_request()->

    #http_request{method=get,
		  uri="TheAnswerMachine/arts/books?firstName=Milan&lastName=Kundera&country=France&country=Czechoslovakia",
		  version="1.1",
		  headers=[{"Content-Length","1234"},{"Accept","text/html"}],
		  params=[],
		  body=undefined}.


%% @private
generate_base_post_request()->

    #http_request{method=post,
		  uri="TheAnswerEngine/arts/books",
		  version="1.1",
		  headers=[{"Content-Length","1234"},{"Accept","text/html"}],
		  params=[],
		  body="firstName=Milan&lastName=Kundera&country=France&country=Czechoslovakia"}.



