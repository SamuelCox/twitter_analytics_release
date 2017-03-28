-module(twitter_stream).
-compile(export_all).

%Starts the analytics_stream gen_server,
%and inits the twitter api stream.
start_stream({Consumer, AccessParams}) ->
    analytics_stream:start_link(),    
    stream({Consumer, AccessParams}),
    io:format("starting receive").
      



%A receive loop that receives chunked http
%responses from the twitter api, sends them
%to the processor process and the receives more.
stream_receive_loop(Processor, IncompleteTweet) ->
    receive
        {http, {_RequestId, stream_start, _Headers}} ->                        
            stream_receive_loop(Processor, []);
        
        {http, {_RequestId, stream, BinBodyPart}} ->            
            Chunk = binary_to_list(BinBodyPart),
            AppendedChunk = string:concat(IncompleteTweet, Chunk),            
            case check_full_tweet(AppendedChunk) of
                true -> 
                    Processor !  {process, AppendedChunk},
                    stream_receive_loop(Processor, []);
                false -> stream_receive_loop(Processor, AppendedChunk)
            end;
            
            
            
        {http, {_RequestId, stream_end, _Headers}} ->
            ack
    end.


%A process that processes chunks of twitter data.
data_processor() ->
    receive
        {process, Chunk} ->
            process_chunk(Chunk)
    end,
    data_processor().

% Checks the Chunk returned by the stream API 
% is a full tweet.
check_full_tweet(Chunk) ->    
    case string:str(Chunk,"\r\n") of 
        0 -> false;
        _ -> true
    end.

%Processes a single Chunk of data, and runs analytics on it.
process_chunk(Chunk) ->    
    Tweets = string:tokens(Chunk, "\r\n"),
    DecodedTweets = lists:map(fun(X) -> {_, Return} = mochijson2:decode(X),
                        Return end, Tweets),
    RealTweets = lists:map(fun(X) -> proplists:delete(<<"delete">>, X) end, DecodedTweets),    
    FormattedTweets = lists:map(fun(X) -> twitter:format_tweets_response(X) end, RealTweets),
    S = self(),
    SPid = spawn(twitter_stream, map_sentiment, [S, FormattedTweets]),
    WPid = spawn(twitter_stream, get_and_combine_words, [S, FormattedTweets]),    
    Count = length(FormattedTweets),
    receive
        {SPid, AggregateSentiment} ->
            ack
    end,
    receive
        {WPid, OverallWords} ->
            ack
    end,
    A = analytics_stream:analyse({Count, OverallWords, AggregateSentiment}),        
    websocket ! {text, A}.

    
% Does sentiment analyises on a list of tweets.    
map_sentiment(Sender, Tweets) ->
    Sentiment = lists:map(fun(X) -> analytics:find_sentiment(X) end, Tweets),
    AggregateSentiment = analytics:aggregate_sentiment(Sentiment),
    Sender ! {self(), AggregateSentiment}.



% Finds the common words in a List of tweets.
get_and_combine_words(Sender, Tweets) ->
    Words = lists:map(fun(X) -> words(X) end, Tweets),
    OverallWords = lists:foldl(fun(X, Acc) -> analytics:combine_maps(X, Acc) end, maps:new(), Words),
    Sender ! {self(), OverallWords}.

% Finds the words in a single tweet.
words(Tweet) ->    
    Words = string:tokens(Tweet, " "),
    analytics:count_words([Words]).

% Sends a streaming request to the Twitter API, and
% sets up a process to received the streamed chunked HTTP response.
stream({Consumer, AccessParams}) ->
    URL = "https://stream.twitter.com/1.1/statuses/filter.json",
    AccessToken = oauth:token(AccessParams),
    AccessTokenSecret = oauth:token_secret(AccessParams),
    ExtraParams = [{"locations", "-122.75,36.8,-121.75,37.8"}],                                                            
    Processor = spawn(twitter_stream, data_processor, []),    
    R = spawn(fun() -> stream_receive_loop(Processor, []) end),
    Options = [{stream, self}, {sync, false}, {receiver, R}],
    oauth:get(URL, ExtraParams, Consumer, AccessToken, AccessTokenSecret, Options).    