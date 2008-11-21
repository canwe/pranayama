
%%@doc
%%
%% The global name of the Registrar; required so that the webapps
%% can register themselves.
%%
%% @end
-define(PRANAYAMA_REGISTRAR,pranayama_registrar).

%%@doc
%%
%% In order to see the any global name it seems necessary
%% to at least ping the node.
%%
%% @end
-define(PRANAYAMA_NODE,pranayama).


%%%-------------------------------------------------------------------------------
%%% Data type: web_app_config - How a registered webapp describes itself.
%%% where:
%%%   alias: atom() Presumed to be unique among the registered webapps; this 
%%%          corresponds to #http_request.web_app_alias
%%%   global_process_name: All webapps must be globally registered processes.
%%%-------------------------------------------------------------------------------
-record(web_app_config,
	{alias,
	 global_process_name}).

%%%-------------------------------------------------------------------------------
%%% Data type: http_request
%%% where:
%%%    method: atom() [get,post]
%%%    uri:    string() The full uri, 
%%%            i.e. http://host/some_web_app_alias/a/b?key1=value1
%%%    version: string() Version of HTTP that the browser supports 
%%%    headers: [{atom()=Key,Value=string()},[{},{}] ...] All http headers
%%%    params: [{atom()=Key,Value=string()},[{},{}] ...] All query params,
%%%            including any from the posted body.
%%%    body: binary(),
%%%    web_app_alias: atom() Used to determine the appropriate web app to which to
%%%                   delegate the handling of this request. In the example uri
%%%                   above it would be 'some_web_app_alias'.
%%%    web_app_path:  The portion of the path that follows the web_app_alias but
%%%                   excluding any query params; i.e. /a/b. 
%%%-------------------------------------------------------------------------------
-record(http_request,
	{method,
	 uri,
	 version,
	 headers=[],
	 params=[],
	 body=[],
	 web_app_alias,
	 web_app_path,
	 response_format,
	 client_ip,
	 client_port,
	 client_username,
	 client_description
	 }).

%%%-------------------------------------------------------------------------------
%%% Data type: http_response (As returned by the webapp that handled 
%%%            the http_request; it is then converted into the 'real'
%%%            Http Response.
%%% where:
%%%    status: atom() Represents the generic http response code which is later
%%%            translated into the corresponding numeric code. 
%%%            Currently used: ok, created, bad_request, server_error, not_found
%%%    content_type: atom() Converted into the corresponding Content Type
%%%            for the 'real' Http Response; for example: xml becomes "text/xml";
%%%            Currently used: html,json, jpg, png, text, xml.
%%%    body:   binary()     
%%%-------------------------------------------------------------------------------
-record(http_response,
	{status,
	 status_code_num,
	 status_code_text,
	 content_type,
	 body,
	 client_ip,
	 client_username,
	 client_description,
	 client_version,
	 request_method,
	 requested_full_path, % Includes leading /
	 content_length
	}).
