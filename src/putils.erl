-module(putils).
-author("Samuel Cox").
-export([p_map/2]).

% Maps a function over a List in parallel, i.e 
% the function doesn't block, and waits for it
% to have been completed over the whole list before
% returning.
p_map(F, List) ->
    S = self(),
    Ids = lists:map(fun(X) ->
       spawn(fun() -> 
        singular_function_call(S, F, X) 
        end) 
        end,
       List),
    retrieve_all(Ids).

% Used to apply a function to a singular element in a List.
singular_function_call(Sender, F, X) ->
    Val = F(X),
    Sender ! {self(), Val}.

%Retrieves all the results of the function calls.
retrieve_all([]) ->
    [];
retrieve_all([X | Xs]) ->
    receive
        {X, Val} ->
            [Val | retrieve_all(Xs)]
    end.




