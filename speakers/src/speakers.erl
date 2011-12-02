%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc speakers startup code

-module(speakers).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0, trace/0]).


trace() ->
    {ok, Cwd} = file:get_cwd(),
    wmtrace_resource:add_dispatch_rule("wmtrace", filename:join([Cwd,"trace"]) ).



ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    speakers_sup:start_link().

%% @spec start() -> ok
%% @doc Start the speakers server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(speakers).

%% @spec stop() -> ok
%% @doc Stop the speakers server.
stop() ->
    Res = application:stop(speakers),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
