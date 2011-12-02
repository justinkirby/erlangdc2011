%%%-------------------------------------------------------------------
%%% File    : speakers_model.erl
%%% Author  : Justin Kirby <>
%%% Description :
%%%
%%% Created : 30 Nov 2011 by Justin Kirby <>
%%%-------------------------------------------------------------------
-module(speakers_model).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create/1,
         read/1,
         exist/1,
         delete/1,
         validate/1,
         to_pl/1,
         to_uri/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(speakers, {
          name :: string(),
          talk :: string(),
          start,
          'end'
          }).

-record(state, {
          tid = undefined
         }).

-define(SERVER,?MODULE).


-include("speakers.hrl").

%%====================================================================
%% API
%%====================================================================


create(Data) ->
    gen_server:cast(?SERVER, {create, Data}).

read(all) ->
    gen_server:call(?SERVER, {read, all});
read(Name) ->
    gen_server:call(?SERVER, {read, Name}).


exist(Name) ->
    gen_server:call(?SERVER, {exist, Name}).

delete(Name) ->
    gen_server:call(?SERVER, {delete, Name}).


validate(Data) ->
    do_validate(Data).

to_pl(Data) ->
    do_pl(Data).

to_uri(Data) ->
    do_uri(Data).




%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->

    Tid = ets:new(speakers, [named_table,
                             set,
                             private,
                             {keypos, #speakers.name}]),

    [ets:insert(Tid,S) || S <- fake_data()],

    {ok, #state{tid = Tid}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({exist, Name}, _From, #state{tid = Tid} = State) ->
    NameDecoded = http_uri:decode(Name),
    case ets:lookup(Tid, NameDecoded) of
        Result when length(Result) > 0 ->
            {reply, true, State};
        _ ->
            {reply, false, State}
    end;

handle_call({read, all}, _From, #state{tid = Tid} = State) ->
    {reply, ets:tab2list(Tid), State};

handle_call({read, Name}, _From, #state{tid = Tid} = State) ->
    NameDecoded = http_uri:decode(Name),
    case ets:lookup(Tid, NameDecoded) of
        Result when length(Result) == 1 ->
            {reply, hd(Result), State};
        _ ->
            {reply, undefined, State}
    end;
handle_call({delete, Name}, _From, #state{tid = Tid} = State) ->
    NameDecoded = http_uri:decode(Name),
    try
        ets:delete(Tid, NameDecoded),
        {reply, ok, State}
    catch
        _:_  ->
            {reply, error, State}
    end.



%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, Pl}, #state{tid = Tid} = State) ->
    Rec = do_rec(Pl),
    ets:insert(Tid, Rec),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_validate(Data) ->
    %% must be a proplist and have all the following keys
    do_validate([<<"name">>,<<"talk">>,<<"start">>,<<"end">>], Data,true).

do_validate([],_,Valid) -> Valid;
do_validate([K|Ks], Data, Valid) ->
    case lists:member(K,proplists:get_keys(Data)) of
        true ->
            do_validate(Ks, Data, Valid);
        false ->
            ?DEBUG("~p not in ~p~n",[K,Data]),
            false
    end.

do_pl(#speakers{} = Data) ->
    [{<<"name">>, ensure_bin(Data#speakers.name)},
     {<<"talk">>, ensure_bin(Data#speakers.talk)},
     {<<"start">>, ensure_bin(Data#speakers.start)},
     {<<"end">>, ensure_bin(Data#speakers.'end')}].


do_uri(Data) when is_list(Data) ->
    [do_uri(D) || D <- Data];
do_uri(#speakers{name = Name} = _State) ->
    list_to_binary(["/speakers/",http_uri:encode(Name),"/"]).


do_rec(Pl) ->
    #speakers{
        name = binary_to_list(proplists:get_value(<<"name">>,Pl)),
        talk = binary_to_list(proplists:get_value(<<"talk">>,Pl)),
        start = binary_to_list(proplists:get_value(<<"start">>,Pl)),
        'end' = binary_to_list(proplists:get_value(<<"end">>,Pl))
        }.


ensure_bin(L) when is_list(L) -> list_to_binary(L);
ensure_bin(L) -> L.

fake_data() ->
    [
     #speakers{name = "Francesco Cesarini",
               talk = "Morning Bootcamp: Practical Erlang Programming",
               start = "Sat, 02 Dec 2011 09:00:00",
               'end' = "Sat, 02 Dec 2011 12:00:00"
              },
     #speakers{name = "Colin MacDonald",
               talk = "Crafty Erlang - An Elegant Tool for Small Projects",
               start = "Sat, 02 Dec 2011 13:10:00",
               'end' = "Sat, 02 Dec 2011 13:40:00"
              },
     #speakers{name = "Eric Merritt",
               talk = "Building Enterprise Erlang Applications with Sinan",
               start = "Sat, 02 Dec 2011 14:10:00",
               'end' = "Sat, 02 Dec 2011 14:40:00"
              },
     #speakers{name = "Paul Davis",
               talk = "Erlang in Production",
               start = "Sat, 02 Dec 2011 16:10:00",
               'end' = "Sat, 02 Dec 2011 16:40:00"
              },
     #speakers{name = "Rusty Klophaus",
               talk = "Everybody Polyglot!",
               start = "Sat, 02 Dec 2011 15:40:00",
               'end' = "Sat, 02 Dec 2011 16:10:00"
              },
     #speakers{name = "Ryan Zezeski",
               talk = "From Java to Erlang",
               start = "Sat, 02 Dec 2011 13:40:00",
               'end' = "Sat, 02 Dec 2011 14:10:00"
              },
     #speakers{name = "Bryan Hunter",
               talk = "Erlang for C# Developers",
               start = "Sat, 02 Dec 2011 17:00:00",
               'end' = "Sat, 02 Dec 2011 17:30:00"
              },
     #speakers{name = "Yurii Rashkovskii",
               talk = "Deploying Erlang Applications",
               start = "Sat, 02 Dec 2011 17:40:00",
               'end' = "Sat, 02 Dec 2011 18:10:00"
              },
     #speakers{name = "Justin Kirby",
               talk = "Making erlang speak REST using Webmachine",
               start = "Sat, 02 Dec 2011 15:00:00",
               'end' = "Sat, 02 Dec 2011 16:40:00"
              }
    ].

