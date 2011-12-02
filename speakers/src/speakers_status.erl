%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(speakers_status).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("speakers.hrl").

init([]) ->
    {ok, undefined};
init([trace]) ->
    {ok, Cwd} = file:get_cwd(),
    {{trace, filename:join([Cwd,"trace"])},undefined}.

allowed_methods(ReqData, State) ->
    ?TRACE(allowed_methods,"",[]),
    {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    ?TRACE(content_types_provided,"",[]),
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, State) ->
    ?TRACE(to_json,"",[]),
    DataPl = [speakers_model:to_pl(Data) || Data <- speakers_model:read(all)],
    Statuses = [make_status(D) || D <- DataPl],

    StatusJson = filter_status(wrq:path_info(filter, ReqData), Statuses),

    {mochijson2:encode(StatusJson),ReqData, State}.



filter_status(Filter, Statuses) ->
    FilterA = list_to_atom(Filter),
    [list_to_binary(["/speakers/",Name,"/"]) ||
        {Name, Status} <- Statuses,
        begin
            Status == FilterA
        end].


make_status(Pl) ->
    Now = now_secs(),
    End = date_secs(proplists:get_value(<<"end">>,Pl)),
    Start = date_secs(proplists:get_value(<<"start">>,Pl)),

    Status = case Start > Now of
                 true ->
                     waiting;
                 false ->
                     case Start < Now andalso End > Now of
                         true ->
                             speaking;
                         false ->
                             done
                     end
             end,

    {proplists:get_value(<<"name">>, Pl), Status}.


now_secs() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(erlang:now())).


date_secs(Date) ->
    calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(binary_to_list(Date))).
