%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% Unit test suite - at least a beginning.
%%% 
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
%%% @todo More tests.
%%%-------------------------------------------------------------------

-module(test_pranayama).

-export([test/0]).


%%--------------------------------------------------------------------
%% @spec test()->ok
%%       
%% @doc
%% 
%%
%% @end
%%--------------------------------------------------------------------
test()->

    pranayama_utils:test(),
    pranayama_registrar:test(),
    pranayama_request_reader:test(),
    pranayama_response_writer:test(),
    pranayama_conversation:test(),
    
    ok.
