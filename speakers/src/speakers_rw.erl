%% @author Justin Kirby <jkirby@voalte.com>
%% @copyright 2011 Justin Kirby.
%% @doc ErlangDC PUT,DELETE example, comments relect differences from
%% speakers_ro.erl

-module(speakers_rw).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         malformed_request/2,
         delete_resource/2,
         to_json/2,
         from_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("speakers.hrl").

%% same as speakers_ro
init([]) ->
    {ok, undefined};
init([trace]) ->
    {ok, Cwd} = file:get_cwd(),
    {{trace, filename:join([Cwd,"trace"])},undefined}.

%% added 'PUT' and 'DELETE', otherwise a PUT request would be
%% responded to with 405 Method Not Allowed
allowed_methods(ReqData, State) ->
    ?TRACE(allowed_methods,"~p~n",[wrq:method(ReqData)]),
    {['GET','PUT', 'DELETE'], ReqData, State}.

%% same as speakers_ro
content_types_provided(ReqData, State) ->
    ?TRACE(content_types_provided,"",[]),
    {[{"application/json", to_json}], ReqData, State}.

%% This lists the mimetypes we accept, there can be more than one. WM
%% will try to match with what is provided in Content-Type header. If
%% no match then an error is returned. If the client did specify a
%% Content-Type that is listed here, then the corresponding function
%% will be the ultimate destination, assuming all other paths are
%% successful. Note that from_json may never be called, just like
%% to_json in content_types_provided.
content_types_accepted(ReqData, State) ->
    ?TRACE(content_types_accepted,"",[]),
    {[{"application/json", from_json}], ReqData, State}.


%% determine whether a resource exists. if not then return false. if
%% it does exist, return true. Note that if the name is undefined, it
%% was not specified so return a list of all url paths. which means
%% this resource ALWAYS exists, even if empty.
%%
%% Also this can come into play with POST and post_is_create. That can
%% get really confusing so I am avoiding it in this example.
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


%% use whatever semantics your app requires to determine if the
%% request is valid here. This should really just focus on the data
%% coming in, i.e validating the body if it exists and/or looking at
%% query params.
%%
%% if the data is valid you should return false, i.e. it is NOT
%% malformed.  returning true here means that this IS a malformed
%% request. And the client will get a 400 Bad Request response.
malformed_request(ReqData, State) ->
    ?TRACE(malformed_request,"",[]),
    case wrq:method(ReqData) of
        'PUT' ->
            case wrq:req_body(ReqData) of
                <<>> ->
                    %% if this is a PUT and no body, bad bad
                    {true, ReqData, State};
                Body ->
                    %%We have a body! make sure it is sane json
                    case mochijson2:decode(Body) of
                        {struct, Pl} ->

                            %% w00t sane json, now is all the data there?
                            Valid = not speakers_model:validate(Pl),

                            {Valid, ReqData, State};
                        _ ->
                            %% bad json, bad client, thwap
                            {true,ReqData, State}
                    end
            end;
        'DELETE' ->
            %% we validate the delete request in that the client
            %% should delete a resource, not the 'collection'
            case wrq:path_info(name, ReqData) of
                undefined ->
                    %% the client tried to delete /speakers. This
                    %% makes no sense.
                    {true, ReqData, State};
                _ ->
                    %% the client is trying to delete a
                    %% /speakers/NAME, good
                    {false, ReqData, State}
            end;
        _ ->
            %% HEAD,OPTIONS,PATCH,BOB,GET do not care about.
            {false, ReqData, State}
    end.



%% is the wrq:method/1 is DELETE WM will end up here. This is where
%% the actual operation should be performed.
delete_resource(ReqData, State) ->
    ?TRACE(delete_resource,"",[]),
    Name = wrq:path_info(name, ReqData),
    case speakers_model:delete(Name) of
        ok ->
            {true, ReqData, State};
        _ ->
            {false, ReqData, State}
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

from_json(ReqData, State) ->
    ?TRACE(from_json, "",[]),

    {struct, Pl} = mochijson2:decode(wrq:req_body(ReqData)),
    speakers_model:create(Pl),
    {true, ReqData, State}.
