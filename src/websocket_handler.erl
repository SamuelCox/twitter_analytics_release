-module(websocket_handler).
-author("Samuel Cox").
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


%Inits the module
init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

% Registers the process for websocket data to be sent to.
websocket_init(State) ->
    io:format("Init called", []),
    register(websocket, self()),
    io:format("~s init", [pid_to_list(whereis(websocket))]),
	{ok, State}.

%Starts of the twitter streaming.
websocket_handle({text, Msg}, State) ->
    io:format("Got message", []),    
    [{_, C}] = ets:lookup(tweets_table, "token"),
    io:format("Starting",[]),    
    spawn(twitter_stream, start_stream, [C]),    
    {reply, {text, <<"\"Started\"">>}, State};
websocket_handle(_, State) ->
	{ok, State}.

% Receives a message from another process, and sends this data
% over websockets to the client.
websocket_info({text, {Count, Words, Sentiment}}, State) ->
    io:format("Sending message", []),    
    BinaryProps = lists:map(fun({X, Y}) -> {list_to_binary(X), Y} end, Words),
    {Good, Bad, Equal} = Sentiment,
    io:format("About to encode", []),
    ReplyJson = jsx:encode([{<<"count">>, Count}, {<<"words">>, BinaryProps},
                            {<<"good">>, Good},
                            {<<"bad">>, Bad},
                            {<<"equal">>, Equal}]),
    io:format("About to send actual frame", []),
    {reply, {text, ReplyJson}, State}.

