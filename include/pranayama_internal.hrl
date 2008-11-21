


-define(REQUEST_METHODS,[{"DELETE",delete},
			 {"GET",get},
			 {"HEAD",head},
			 {"POST",post},
			 {"PUT",put}
                         
			].
-define(FALLBACK_REQUEST_METHOD,get).

-define(RESPONSE_FORMATS,[{"html",html},
			  {"json",json},
			  {"text",text},
			  {"xml",xml}
			  
			 ].

-define(FALLBACK_RESPONSE_FORMAT,html).
-define(FORMAT_PARAM_KEY,"format").
-define(CLIENT_DESC_HEADER_KEY,"User-Agent").


-define(CONTENT_TYPES,[
	{aiff,"audio/aiff"},
	{jpg,"image/jpeg"},
	{png,"image/png"},
	{json,"application/json"},
	{html,"text/html"},
	{text,"plain/text"},			       
	{xml,"text/xml"}
	]).

-define(FALLBACK_CONTENT_TYPE,"text/html").

-define(STATUS_CODES,
	[{ok,{200,"OK"}},
	 {created,{201,"Created"}},
	 {bad_request,{400,"Bad Request"}}, %% Client's fault. Body should contains details 
	 {unauthorized,{401,"Unauthorized"}},
	 {not_found,{404,"Not Found"}}, %% Request for an unknown resource
	 {method_not_allowed,{405,"Method Not Allowed"}},
	 {conflict,{409,"Conflict"}},
	 {unsupported_request_body_format,{415,"Unsupported Media Type"}},
	 {internal_error,{500,"Internal Server Error"}},
	 {service_unavailable,{503,"Service Unavailable"}}
		      ]).
-define(FALLBACK_STATUS_CODE,{400,"Bad Request"}).		      


%% Please see /doc/pranayama_types.edoc for commentary

-record(app_config,
	{port_num,
	 web_app_registration_process_name,
	 receive_request_timeout_ms,
	 handle_request_timeout_ms,
	 log_directory
	}).


-record(log_message,
	{level,
	template,
	params=[]}).
