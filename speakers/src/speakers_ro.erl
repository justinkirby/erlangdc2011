%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(speakers_ro).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("speakers.hrl").

%% this is the init fun called when a webmachine decides to dispatch
%% to this module based on informaiton from the dispatch.conf. The
%% second element in the tuple is the state/context that is passed as
%% the second param to the other funs, e.g. state
init([]) ->
    {ok, undefined};
init([trace]) ->
    {ok, Cwd} = file:get_cwd(),
    {{trace, filename:join([Cwd,"trace"])},undefined}.

%% restrict the allowable methods to just GET
allowed_methods(ReqData, State) ->
    ?TRACE(allowed_methods,"",[]),
    {['GET'], ReqData, State}.

%% only support json and map that to to_json/2
content_types_provided(ReqData, State) ->
    ?TRACE(content_types_provided,"",[]),
    {[{"application/json", to_json}], ReqData, State}.


%% determine whether a resource exists. if not then return false. if
%% it does return true. Note that if the name is undefined, it was not
%% specified so return a list of all url paths. which means this
%% resource ALWAYS exists, even if empty
resource_exists(ReqData, State) ->
    case wrq:path_info(name, ReqData) of
        undefined ->
            %% there was no name specified, so requesting all
            {true, ReqData, State};
        Name ->
            Exists = speakers_model:exist(Name),
            ?TRACE(resource_exists,"~p~n",[Exists]),
            {Exists, ReqData, State}
    end.



%% this will always be called on a GET. determine if the name was in
%% the path, if not return a list of url paths, if name was specified
%% return a dictionary of that resource.
%%
%% /speakers -> [path1,...,pathN]
%% /speakers/full%20name -> {k1:v1,...,kN,vN}
to_json(ReqData, State) ->
    ?TRACE(to_json,"",[]),
    case wrq:path_info(name, ReqData) of
        undefined ->
            %% did not get a name, so return a list of paths available.
            Data = speakers_model:read(all),
            Uris = speakers_model:to_uri(Data),
            {mochijson2:encode(Uris), ReqData, State};
        Name ->

            %% a specific name was requested, so return the dictionary
            %% for that name
            Data = speakers_model:read(Name),
            DataPl = speakers_model:to_pl(Data),

            %% note the hack: if you have a proplist, then you can
            %% just wrap it in a struct to make mochijson2 accept
            %% it. All string data MUST be binary and NOT list though
            {mochijson2:encode({struct, DataPl}), ReqData, State}
    end.
