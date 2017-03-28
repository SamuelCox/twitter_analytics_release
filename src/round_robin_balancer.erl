-module(round_robin_balancer).
-behaviour("Samuel Cox").
-behaviour(gen_server).



-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3, node_call/3, start_link/0]).





%partial implementation of a round robin load balancer.
node_call(ModuleName, FunctionName, Args) ->
    Node = gen_server:call(?MODULE, "node"),
    case Node of
        no_node ->
            spawn(ModuleName, FunctionName, Args);
        _ ->
            spawn(Node, ModuleName, FunctionName, Args)
    end.

            

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    {ok, 1}.

%A partial implementation of a round robin load balancer.
handle_call("node", _From, State) ->
    Nodes = nodes(),
    Len = length(Nodes),
    case Len of
        0 -> no_node;
        _ ->
            Index = State rem Len,
            case Index of 
                0 ->
                    Node = lists:nth(Len, Nodes),
                    {reply, Node, 1};
                _ ->
                    Node = lists:nth(Index, Nodes),
                    {reply, Node, Index + 1}
            end
    end.
    

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_, _State) -> ok.

code_change(_, State, _Extra) -> {ok, State}.

    



