
%% When making a release, you must change the conf file name to /etc/pranayama.conf
%% When you go back to development, change it back to ./etc/pranayama.conf and run 'makeboot'

{application,pranayama,
[{description,"Pranayama (Basic Web App Server)"},
 {vsn,"0.1.0"},
 
 %% There are some files we don't need for a release: 
 %% pranayama_dev, test_pranayama, test_pranayama_utils
 
 {modules,[	pranayama_app,
		pranayama_conversationalist,
		pranayama_conversation,
		pranayama_event_manager,
		pranayama_event_manager_sup,
		pranayama_log,
		pranayama_logger,
		pranayama_log_writer,
		pranayama_net,
		pranayama_registrar,
		pranayama_request_reader,
		pranayama_response_writer,
		pranayama_spy,
		pranayama_sup,
		pranayama_utils
		]},
 {registered,[	
		pranayama_conversationalist,
		pranayama_event_manager_sup,
		pranayama_event_manager,
		pranayama_log,
		pranayama_log_writer,
		pranayama_registrar,
		pranayama_sup,
		pranayama_spy
		]
		},
 {applications,[kernel,stdlib,sasl]},
 {mod,{pranayama_app,["./etc/pranayama.conf"]}},
 {start_phases,[]}			 
			 
]}.