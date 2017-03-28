-module(analytics_stream).
-author("Samuel Cox").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3, analyse/1, start_link/0]).


-behaviour(gen_server).



% Top-level call for the gen-server,
% passed in the analytics of a single tweet,
% it gives the cumulative analytics for all
% past tweets and the new tweet.
analyse({Count, WordMap, Sentiment}) ->
        gen_server:call(?MODULE, {analyse, Count, WordMap,
                                        Sentiment}).
            
% How the gen_server is started, conforms to gen_server spec.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% Inits the state.
init([]) ->
    {ok, {0, maps:new(), {0,0,0}}}.

% Function that actually performs the cumulative analytics on the new data
% and state held in the gen_server.
handle_call({analyse, NewCount, NewWordMap, NewSentiment},
            _From, {OldCount, OldWordMap, OldSentiment}) ->
    
    ReturnCount = OldCount + NewCount,
    S = self(),
    Pid = spawn(fun() -> S ! {self(), analytics:combine_maps(NewWordMap, OldWordMap)} end),
    
    ReturnSentiment = analytics:sum_two_sentiment(OldSentiment, NewSentiment),
    receive
        {Pid, ReturnWords} ->
            ack
    end,
    NewState = {ReturnCount, ReturnWords, ReturnSentiment},
    {reply, {ReturnCount, analytics:trim_map(ReturnWords), ReturnSentiment}, NewState}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_, _State) -> ok.

code_change(_, State, _Extra) -> {ok, State}.

    



