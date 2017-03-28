-module(twitter).
-author("Samuel Cox").
-compile(export_all).

% Inits all ets tables needed for this module's lifetime.
init() ->    
    ets_helpers:new_tweets_table(),
    ets:new(neg_words_table, [public, ordered_set, named_table]),
    ets:new(pos_words_table, [public, ordered_set, named_table]),
    {ok, BinaryNegFile} = file:read_file("../../negative-words.txt"),
    NegFileAsString = unicode:characters_to_list(BinaryNegFile),
    {ok, BinaryPosFile} = file:read_file("../../positive-words.txt"),
    PosFileAsString = unicode:characters_to_list(BinaryPosFile),
    NegWords = string:tokens(NegFileAsString, "\r\n"),
    PosWords = string:tokens(PosFileAsString, "\r\n"),
    lists:foreach(fun(X) -> ets:insert(neg_words_table, {X,X}) end, NegWords),
    lists:foreach(fun(X) -> ets:insert(pos_words_table, {X,X}) end, PosWords).


% Allows a user to receive an oauth token to connect to twitter
% APIs with. Needs you to specify your consumer key and consumer secret
% found in your Twitter App.
authenticate(ConsumerKey, ConsumerSecret) ->
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    RequestTokenURL = "https://api.twitter.com/oauth/request_token",
    {ok, RequestTokenResponse} = oauth:get(RequestTokenURL, [], Consumer),

    RequestTokenParams = oauth:params_decode(RequestTokenResponse),
    RequestToken = oauth:token(RequestTokenParams),
    RequestTokenSecret = oauth:token_secret(RequestTokenParams),
    AuthorizePinURL = "https://api.twitter.com/oauth/authorize?oauth_token=~s",
    io:format(AuthorizePinURL ++ "~n", [RequestToken]),
    AuthVerifierPin = io:get_line("Paste your PIN and press ENTER: "),
    
    AccessTokenURL = "https://api.twitter.com/oauth/access_token",
    {ok, AccessTokenResponse} = oauth:get(AccessTokenURL, [{"oauth_verifier", AuthVerifierPin}], Consumer, RequestToken, RequestTokenSecret),

    {Consumer, oauth:params_decode(AccessTokenResponse)}.

% Wrapper function for sending a request to Twitter APIs.
% This requires your oauth token as the {Consumer, AccessParams} tuple.
request(URL, Params, {Consumer, AccessParams}) ->
    AccessToken = oauth:token(AccessParams),
    AccessTokenSecret = oauth:token_secret(AccessParams),

    {ok, {_Status, _Headers, ResponseBody}} = oauth:get(URL, Params, Consumer, 
                                                        AccessToken, AccessTokenSecret),
    ResponseBody.

% A function that looks up the tweet responses in the cache, and finds
% the 20 most common words in them.
get_most_common_words() ->
    Lookup = ets:lookup(tweets_table, "Tweets"),
    Tweets = lists:append(proplists:get_all_values("Tweets", Lookup)),
    Words = lists:map(fun(X) -> string:tokens(X, " ") end, Tweets),
    analytics:count_words(Words).

% A function that looks up tweet responses in the cache,
% and finds the percentage of retweets and favourites, as well as
% the highest number of retweets and favourites.
get_extra_analytics() ->
    Lookup = ets:lookup(tweets_table, "Responses"),
    Structs = lists:append(proplists:get_all_values("Responses", Lookup)),
    Tweets = proplists:get_all_values(struct, Structs),
    S = self(),
    RtPctPid = spawn(twitter, percent_rt_sup, [S, Tweets]),
    RtCountPid = spawn(twitter, rt_count_sup, [S, Tweets]),
    FavPctPid = spawn(twitter, percent_fav_sup, [S, Tweets]),
    FavCountPid = spawn(twitter, fav_count_sup, [S, Tweets]),
    receive 
        {RtPctPid, RtPct} ->
            ack
    end,
    receive
        {RtCountPid, RtCount} ->
            ack
    end,
    receive
        {FavPctPid, FavPct} ->
            ack
    end,
    receive
        {FavCountPid, FavCount} ->
            ack
    end,
    {RtPct, RtCount, 
     FavPct, FavCount}.

%Supervisor function for message passing.
percent_rt_sup(Sender, Tweets) ->
    Sender ! {self(), analytics:get_percent_retweets(Tweets)}.
%Supervisor function for message passing.
rt_count_sup(Sender, Tweets) ->
    Sender ! {self(), analytics:get_highest_retweets(Tweets)}.
%Supervisor function for message passing.
percent_fav_sup(Sender, Tweets) ->
    Sender ! {self(), analytics:get_percent_favorite(Tweets)}.
%Supervisor function for message passing.
fav_count_sup(Sender, Tweets) ->
    Sender ! {self(), analytics:get_highest_favorites(Tweets)}.

%Determines if a tweet has been favourited.
is_favorite(Tweet) ->
    {_, Fav} = lists:keyfind(<<"favorited">>, 1, Tweet),
    Fav.

%Finds the favourite count of a tweet.
favorite_count(Tweet) ->
    {_, FavCount} = lists:keyfind(<<"favorite_count">>, 1, Tweet),
    FavCount.

%Determines if a tweet has been retweeted.
is_retweet(Tweet) ->
    {_, Retweet} = lists:keyfind(<<"retweeted">>, 1, Tweet),
    Retweet.

%Finds the retweet count of a tweet.
retweet_count(Tweet) -> 
    {_, RtCount} = lists:keyfind(<<"retweet_count">>, 1, Tweet),
    RtCount.
        

% Top-level function that searches the Twitter API for a query,
% and does sentiment analysis on that data as well as filling the cache.
search_and_aggregate(Client, Query, X, AllowCache) ->
    case check_rate_limit("AggrTime") of 
        rate_ok -> check_cache(AllowCache, true, Client, Query, X);
        rate_reached -> check_cache(AllowCache, false, Client, Query, X)
    end.

    
% Checks rate limit has not been exceeded.
check_rate_limit(Key) ->
    Time = erlang:system_time(seconds),
    case ets:lookup(tweets_table, Key) of
        [] -> 
            ets:insert(tweets_table, {Key, Time}),
            rate_ok;
        [{_, Lookup}] -> 
            case Time - Lookup >= 960 of
                true -> rate_ok;
                false -> rate_reached
            end
        
    end.

%Checks the ets tables for cached responses.
check_cache(AllowCache, AllowNewSearch, Client, Query, X) ->
    case AllowCache of
        true ->
            case ets:lookup(tweets_table, "Tweets") of
                [] ->                     
                    new_search_or_fail(AllowNewSearch, Client, Query, X);
                Lookup ->                                
                    Tweets = lists:append(proplists:get_all_values("Tweets", Lookup)),
                    aggregate_from_cache_or_search(Tweets, AllowNewSearch, Client, Query, X)
            end;
        false ->
            new_search_or_fail(AllowNewSearch, Client, Query, X)
    end.

%If a new search can be started, then starts a new search and analyses the sentiment.
%Otherwise returns an error message.    
new_search_or_fail(AllowNewSearch, Client, Query, X) ->
    case AllowNewSearch of
        true ->
            ets:delete(tweets_table, "QueryString"),
            ets:insert(tweets_table, {"QueryString", Query}),
            Aggregate = spawn_search_actors(Client, Query, X);
        false -> "Rate Limit reached, wait 15 minutes"
    end. 


% Given a non-empty cache, does sentiment analysis on the cache.
% Otherwise spawns a new search to do this.
aggregate_from_cache_or_search(Cache, AllowNewSearch, Client, Query, X) ->      
    [{_, CacheQuery}] = ets:lookup(tweets_table, "QueryString"),
    case CacheQuery == Query of
        true -> 
            Sentiment = lists:map(fun(Y) -> analytics:find_sentiment(Y) end, Cache),    
            analytics:aggregate_sentiment(Sentiment);
        false ->
            case AllowNewSearch of
                true ->
                    io:format("deleting query",[]),
                    ets:delete(tweets_table, "QueryString"),
                    io:format("Inserting query ", []),
                    ets:insert(tweets_table, {"QueryString", Query}),
                    ets:delete(tweets_table, "Responses"),
                    ets:delete(tweets_table, "Tweets"),
                    spawn_search_actors(Client, Query, X);
                false ->  "Cache does not match Query, please wait 15 minutes"
            end
    end.
    

% A helper function that spawns the worker processes used
% for divide-and-conquer analytics, and passes them into the
% next function.
spawn_search_actors({Consumer, AccessParams}, Query, X) ->
    Pid1 = spawn(fun() -> decode_search_and_analyse() end),
    Pid2 = spawn(fun() -> decode_search_and_analyse() end),
    Pid3 = spawn(fun() -> decode_search_and_analyse() end),
    Pid4 = spawn(fun() -> decode_search_and_analyse() end),
    Pid5 = spawn(fun() -> decode_search_and_analyse() end),
    spawn_search_actors_r({Consumer, AccessParams}, Query, X, Pid1,
                          Pid2, Pid3, Pid4, Pid5, 0, 0, 0).      



% Recursive function that does requests in parallel (5 at a time),
% analyses these responses and then combines the analytics for each
% response, returning the analytics proper for the whole data set.
spawn_search_actors_r(_, _, 0, 
                      _Pid1,_Pid2,_Pid3,_Pid4, _Pid5,
                       G, B, E) ->
    {G, B, E};
spawn_search_actors_r({Consumer, AccessParams}, Query, X,
                       Pid1, Pid2, Pid3, Pid4, Pid5,
                       Good, Bad, Equal) ->
    S = self(),
    Pid1 ! {S, {Consumer, AccessParams}, Query},    
    Pid2 ! {S, {Consumer, AccessParams}, Query},
    Pid3 ! {S, {Consumer, AccessParams}, Query},
    Pid4 ! {S, {Consumer, AccessParams}, Query},
    Pid5 ! {S, {Consumer, AccessParams}, Query},
            
    
    receive
        {Pid1, Response1} ->            
           ack
    end,
    receive
        {Pid2, Response2} ->
           ack
    end,
    receive
        {Pid3, Response3} ->
           ack
    end,
    receive
        {Pid4, Response4} ->
           ack
    end,
    receive
        {Pid5, Response5} ->
           ack
    end,
    G = element(1, Response1) + element(1, Response2) + element(1, Response3)
    + element(1, Response4) + element(1, Response5),
    
    B = element(2, Response1) + element(2, Response2) + element(2, Response3)
    + element(2, Response4) + element(2, Response5),
    
    E = element(3, Response1) + element(3, Response2) + element(3, Response3)
    + element(3, Response4) + element(3, Response5),    
    spawn_search_actors_r({Consumer, AccessParams}, Query, X-1,
                          Pid1, Pid2, Pid3, Pid4, Pid5,
                          G + Good, B + Bad, E + Equal).


%The function the worker processes run to do the
% http request, json decoding, data transformation and
% analytics.
decode_search_and_analyse() ->
    receive
        {Sender, {Consumer, AccessParams}, Query} ->
            ack
    end,
    {_, DecodedResponse} = mochijson2:decode(search({Consumer, AccessParams}, Query)),    
    {_, Data} = lists:keyfind(<<"statuses">>, 1, DecodedResponse),
    ets:insert(tweets_table, {"Responses", Data}),
    UsefulDataResponse = strip_useless_tweets_data(Data),    
    SafeResponse = lists:map(fun(Y) -> format_tweets_response(Y) end,
    UsefulDataResponse),  

    ets:insert(tweets_table, {"Tweets", SafeResponse}),

    Sentiment = lists:map(fun(Y) -> analytics:find_sentiment(Y) end, SafeResponse),
    Aggregate = analytics:aggregate_sentiment(Sentiment),    
    Sender ! {self(), Aggregate},
    decode_search_and_analyse().

% Normalises a twitter response, gets rid of any weird data.
format_tweets_response(Response) ->
    {_, Text} = lists:keyfind(<<"text">>, 1, Response),
    remove_bad_characters(unicode:characters_to_list(Text,utf8)).

%Removes non utf8 chars from a string.
remove_bad_characters([]) ->
    [];
remove_bad_characters([X | Xs]) when X < 129 andalso X > 0 ->
    [X | remove_bad_characters(Xs)];
remove_bad_characters([X | Xs]) ->
    remove_bad_characters(Xs).

%Gets rid of some noise returned when using json libs.
strip_useless_tweets_data(Statuses) ->
    proplists:get_all_values(struct, Statuses).

%Wrapper function to send a request to the Twitter search API.
search({Consumer, AccessParams}, Query) ->
    request("https://api.twitter.com/1.1/search/tweets.json",
            [{"q", Query}, {"result_type", "recent"},
             {"count","100"}], {Consumer, AccessParams}).

% Gets the WoeID of a latitude and longitude for a location
% on Twitter
get_trends_location(Lat, Long, {Consumer, AccessParams}) ->
    JSON = mochijson2:decode(request("https://api.twitter.com/1.1/trends/closest.json", 
            [{"lat", Lat}, {"long",Long}], {Consumer, AccessParams})),
    [{_, JsonList}] = JSON,
    {_, Id} = lists:keyfind(<<"woeid">>, 1, JsonList),
    Id.

%Wrapper function for getting Trends data.
get_trends(WoeID, {Consumer, AccessParams}) ->
    request("https://api.twitter.com/1.1/trends/place.json",
            [{"id", WoeID}], {Consumer, AccessParams}).

% Supervisor function for json decoding.
parallel_json_supervisor(Sender, Json) ->
    Sender ! {self(), mochijson2:decode(Json)}.

%Finds common trends across England, Germany and America.
find_common_trends({Consumer, AccessParams}) ->
    case check_rate_limit("TrendsTime") of 
        rate_ok -> 
            America = get_trends_location("55.72", "-107.4", {Consumer, AccessParams}),
            England = get_trends_location("52.63", "-1.58", {Consumer, AccessParams}),
            Europe = get_trends_location("52.20", "10.3", {Consumer, AccessParams}),
            AmericaTrends = get_trends(America, {Consumer, AccessParams}), 
            EnglandTrends = get_trends(England, {Consumer, AccessParams}), 
            EuropeTrends = get_trends(Europe, {Consumer, AccessParams}),
            
            S = self(),
            AmID = spawn(fun() -> parallel_json_supervisor(S, AmericaTrends) end),
            EnID = spawn(fun() -> parallel_json_supervisor(S, EnglandTrends) end),
            EuID = spawn(fun() -> parallel_json_supervisor(S, EuropeTrends) end),
            receive 
                {AmID, AJsonMessage} ->
                    [{_, AJsonList}] = AJsonMessage
            end,
            receive 
                {EnID, EnJsonMessage} ->
                    [{_, EnJsonList}] = EnJsonMessage
            end,
            receive 
                {EuID, EuJsonMessage} ->
                [{_, EuJsonList}] = EuJsonMessage
            end,

            AJsonID = spawn(fun() -> strip_supervisor(S, AJsonList) end),
            EnJsonID = spawn(fun() -> strip_supervisor(S, EnJsonList) end),
            EuJsonID = spawn(fun() -> strip_supervisor(S, EuJsonList) end),
            receive 
                {AJsonID, ATrendsData} ->
                    ack
            end,
            receive 
                {EnJsonID, EnTrendsData} ->
                    ack
            end,
            receive 
                {EuJsonID, EuTrendsData} ->
                    ack
            end,

            [X | Xs] = ATrendsData ++ 
            EnTrendsData ++ 
            EuTrendsData,
            Trends = find_duplicates([X | Xs]),
            ets:insert(tweets_table, {"Trends", Trends});
        rate_reached -> 
            [{_, X}] = ets:lookup(tweets_table, "Trends"),
            X
    end.    
    
      

% Supervisor function for stripping noise out of the data returned
% by json libs.
strip_supervisor(Sender, List) ->
    Sender ! {self(), strip_useless_trends_data(List)}.

% Strips un-needed objects from a twitter response.
strip_useless_trends_data(List) ->
    {_, JsonList} =  lists:keyfind(<<"trends">>, 1, List),
    NewList = proplists:get_all_values(struct, JsonList),
    lists:map(fun(X) -> {_, Value} = lists:keyfind(<<"name">>, 1, X), Value end,
    NewList).

% Finds duplicates in a list.
find_duplicates([]) -> 
    [];
find_duplicates([X|Xs]) ->
    case lists:member(X, Xs) of
        true -> [X | find_duplicates(Xs)];
        false -> find_duplicates(Xs)
    end.
    




    
    
    
    
