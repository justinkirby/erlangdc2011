%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the speakers application.

-module(speakers_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for speakers.
start(_Type, _StartArgs) ->
    speakers_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for speakers.
stop(_State) ->
    ok.
