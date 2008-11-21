% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%%  This is not part of the application, but rather provided
%%%  for the convenience of development; it needn't be referenced
%%%  in pranayama.app.
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
-module(pranayama_dev).

-export([doc/0,
	 compile/0,
	 compile_for_tests/0,
	 create_normal_release/0,
	 makeboot/0,
	 test/0,
	 xref/0]).

-define(SRC_DIR,"./src").
-define(DOC_TARGET_DIR,"./doc/edoc").
-define(DOC_SRC_DIR,"./doc/src").
-define(RELEASE_FILE_NAME,"Pranayama-0.1.0").
-define(RELEASE_FILE,"./etc/" ++ ?RELEASE_FILE_NAME).
-define(RELEASE_FILE_FOR_NORMAL_RELEASE,"./releases/normal/" ++ ?RELEASE_FILE_NAME).
-define(RELEASE_FILE_FOR_STANDALONE_RELEASE,"./releases/standalone/" ++ ?RELEASE_FILE_NAME).
-define(ERTS_DIR,"/usr/local/lib/erlang/").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec compile() -> ok
%% @doc
%% It is essential to compile with the NOTEST flag,
%% otherwise <em>pranayama_net</em> will operate in test mode,
%% using 'mocks' and this application will not work.
%%
%% @end
%%--------------------------------------------------------------------
compile()->
    make:all([{d,'NOTEST'}]),
    ok.

%%--------------------------------------------------------------------
%% @spec compile_for_tests() -> ok
%% @doc
%%  Note that we leave out the 'NOTEST' flag; otherwise we wouldn't
%%  be running against the mocks in <em>pranayama_net</em>.
%% @end
%%--------------------------------------------------------------------
compile_for_tests()->
    make:all([]),
    ok.

%%--------------------------------------------------------------------
%% @spec create_normal_release() -> ok
%% @doc
%% By normal release, we mean as opposed to a standalone release which
%% comes with an embedded Erlang runtime. Frankly, I had trouble getting
%% that working.
%% @end
%%--------------------------------------------------------------------
create_normal_release()->
    file:copy(?RELEASE_FILE ++ ".rel",?RELEASE_FILE_FOR_NORMAL_RELEASE ++ ".rel"),
    systools:make_script(?RELEASE_FILE_FOR_NORMAL_RELEASE),
    systools:make_tar(?RELEASE_FILE_FOR_NORMAL_RELEASE,[{dirs,[include,doc]}]),
    ok.


%%--------------------------------------------------------------------
%% @spec doc() -> ok
%% @doc
%% Edocs over all our source files; we include the private methods
%% as well as our todos. We do not, however, include any outside
%% applications; if someone could get that working, that would be swell.
%% @end
%%--------------------------------------------------------------------
doc()->

    SourceFiles = filelib:wildcard(?SRC_DIR ++ "/*.erl"),
    
    edoc:files(SourceFiles,
	       [{new,true}, %% Ignores any existing files and overwrites them
		{dir,?DOC_TARGET_DIR},
		{private,true},
		{todo,true},
		{overview, ?DOC_SRC_DIR ++ "/overview.edoc"}
	       ]),
    ok.

%%--------------------------------------------------------------------
%% @spec makeboot() -> ok
%% @doc
%%  Makes a boot script for local use (as opposed to one suitable for
%%  a release, as we do in create_normal_release()).
%% @end
%%--------------------------------------------------------------------
makeboot()->
    systools:make_script(?RELEASE_FILE,[local]),
    ok.

%%--------------------------------------------------------------------
%% @spec test() -> ok
%% @doc
%%  Our module <em>test_pranayama</em> contains calls to all our unit tests.
%%  
%% @end
%%--------------------------------------------------------------------
test()->
    test_pranayama:test(),
    ok.

    
%%--------------------------------------------------------------------
%% @spec xref() -> ok
%% @doc
%%  Runs xref over all our beams.
%%  
%% @end
%%--------------------------------------------------------------------
xref()->
    io:format("~p~n",[xref:d("ebin")]),
    ok.



