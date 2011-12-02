
-define(ERROR(Msg, Args), io:format(Msg,Args)).
-define(DEBUG(Msg, Args), io:format(Msg,Args)).
-define(TRACE(Fun,Msg, Args), io:format("~p:~p ~p~n"++Msg, [?MODULE,?LINE, Fun|Args])).
