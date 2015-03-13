This Erlang project originated as an offshoot of another Erlang project of mine -  [The (Mock) Exchange](http://www.themockexchange.com); while I could have used something like Yaws, I was intrigued by the idea of building my own server.

My architecture is simple: the server acts as a filter, intercepting the 'raw' http request which it then transforms into an erlang record; this record is sent off to the appropriate delegate; this delegate is any globally registered erlang process which can take responsibility for generating
the response; this response is sent back to Pranayama which
transforms into the 'raw' http response, sending it back to the client.

One of the design aims was to limit the coupling between
the server and the various web apps which used it; besides the inclusion of a single .hrl file, a web app must simply
register with the server; if the server crashes, losing its registration data, all web apps are informed so that they may re-register.

About a year ago, I started to learn Erlang part-time; I learned much from the projects of others and wanted to bring something to the table that might be of similar aid to others.

Feedback is certainly welcome; this is my first participation in an open source project, so I'd appreciate any guidance.

Sean Rasmussen (sean@erasmos.com)