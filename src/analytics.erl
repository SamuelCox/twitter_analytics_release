-module(analytics).
-author("Samuel Cox").
-compile(export_all).

% Lists words we don't want to look at
% when we try to find the most common words.
ignored_words() ->
    ["this", "that", 
     "what", "your", "from",
     "just", "have",  "it's",
     "will",  "they",  "would",
      "when",  "make", "time",
      "their", "there","because",
      "they're","we're", "with",
      "here", "about", "&amp;",
      "you're", "with", "want"].


%Creates a Map of word to value, if it doesn't exist
% it inits the value to 1, otherwise it increments the
% value.
inc_or_put(Word, Map, Value) when length(Word) > 3 ->
    OldValue = maps:get(Word, Map, empty),
    case OldValue of
        empty ->
            maps:put(Word, Value, Map);
        _ ->
            maps:put(Word, OldValue + Value, Map)
    end;
inc_or_put(_Word, Map, _Value) ->
    Map.

% An analytics function that takes a list of strings,
% and counts all the words in the list. Creates
% a map of all words to their values.
count_words(List) ->
  lists:foldl(fun(Sentence, SentenceMap) ->
    lists:foldl(fun(Word, Map) ->
        case lists:member(string:to_lower(Word), ignored_words()) of
            true -> Map;
            false -> inc_or_put(string:to_lower(Word), Map, 1)
        end
    end, SentenceMap, Sentence)
  end, maps:new(), List).

% Helper function to combine two maps,
% since maps are immutable you need to don
% this to make cumulative word maps.
combine_maps(FirstMap, SecondMap) ->    
    SecondKeys = maps:keys(SecondMap),
    WordMap = lists:foldl(fun(X, Map) ->
                inc_or_put(X, Map, 
                           maps:get(X, SecondMap, empty)
                           ) end, FirstMap, SecondKeys).

%Trims a map down to the 20 most common words,
%to avoid a memory leak.
trim_map(Map) ->
    PropList = maps:to_list(Map),
    SortedPropList = lists:keysort(2, PropList),
    Length = length(SortedPropList),
    case Length > 20 of
        true ->
            TrimmedPropList = lists:nthtail(Length - 20, SortedPropList);
        false ->
            SortedPropList
    end.

%Takes in a list of search responses,
% and calculates the percentage that were retweets.
get_percent_retweets(Tweets) ->    
    Length = length(Tweets),
    Retweets = lists:filter(fun(X) -> twitter:is_retweet(X) == true end, Tweets),
    (length(Retweets) / Length) * 100.

%Takes in a list of search responses,
% and calculates the highest amount of retweets seen.
get_highest_retweets(Tweets) ->
    lists:foldl(fun(X, Acc) -> 
                Count = twitter:retweet_count(X),
                case X > Acc of
                    true -> Count;
                    false -> Acc
                end end, 0, Tweets).
%Takes in a list of search responses,
% and calculates the percentage that were favourites.
get_percent_favorite(Tweets) ->
    Length = length(Tweets),
    Favs = lists:filter(fun(X) -> twitter:is_favorite(X) == true end, Tweets),
    (length(Favs) / Length) * 100.


%Takes in a list of search responses,
% and calculates the highest amount of favourites seen.
get_highest_favorites(Tweets) ->
    lists:foldl(fun(X, Acc) -> 
                Count = twitter:favorite_count(X),
                case X > Acc of
                    true -> Count;
                    false -> Acc
                end end, 0, Tweets).

    


% A helper function to tail-recursively aggregate the sentiment_fold
% of a list of sentiments.
aggregate_sentiment_helper([], G, B, E) ->
    {G, B, E};    
aggregate_sentiment_helper([X | Xs], G, B, E) when X == good ->
    aggregate_sentiment_helper(Xs, 1 + G, B, E);
aggregate_sentiment_helper([X | Xs], G, B, E) when X == bad ->
    aggregate_sentiment_helper(Xs, G, 1 + B, E);
aggregate_sentiment_helper([X | Xs], G, B, E) when X == equal ->
    aggregate_sentiment_helper(Xs, G, B, 1 + E).

%Top level function that aggregates the sentiment of a
%list conforming to [equal, good, good, equal, bad]
aggregate_sentiment([]) ->
    {0,0,0};
aggregate_sentiment([X | Xs]) ->
    aggregate_sentiment_helper([X | Xs], 0, 0, 0).

% Takes a string, i.e text data from a tweet response,
% and maps the words into sentiments [good, bad, equal].
% Then sees which sentiment outweighs eachother, and
% returns a single sentiment for a piece of text.
find_sentiment(String) ->    
    Words = string:tokens(string:to_lower(String), " "),
    S = self(),
    Pid1 = spawn(analytics, find_good_sentiment, [S, Words]),
    Pid2 = spawn(analytics, find_bad_sentiment, [S, Words]),    
    receive 
        {Pid1, Good} ->
            ack
    end,
    receive
        {Pid2, Bad} ->
            ack
    end,
    good_outweighs_bad(Good - Bad).

% Simple function that pattern matches
% to decide if sentiment is good, bad or equal.
good_outweighs_bad(0) ->
    equal;
good_outweighs_bad(X) when X > 0  ->
    good;
good_outweighs_bad(X) when X < 0  ->
    bad.

% Helper function that is used in the foldl
% over a list of words to find the sentiment.
sentiment_fold(X, Sum) when X == false ->
    Sum;
sentiment_fold(X, Sum) when X == true ->
    Sum + 1.

% A function that does a parallel non-blocking lookup
% for each of the words in the text data, and determines if
% it is part of the good dictionary. Then folds over the 
% data to sum up the amount of good sentiment.
find_good_sentiment(Sender, Words) ->    
    List = putils:p_map(fun(X) -> ets:member(pos_words_table, X) end, Words),
    Sender ! {self(), lists:foldl(fun(X, Sum) -> sentiment_fold(X,Sum) end, 0, List)}.

% A function that does a parallel non-blocking lookup
% for each of the words in the text data, and determines if
% it is part of the bad dictionary. Then folds over the 
% data to sum up the amount of bad sentiment.    
find_bad_sentiment(Sender, Words) ->    
    List = putils:p_map(fun(X) -> ets:member(neg_words_table, X) end, Words),
    Sender ! {self(), lists:foldl(fun(X, Sum) -> sentiment_fold(X,Sum) end, 0, List)}.


% Just a helper function used for normalising data
% when twitter returns us slightly weird textual data.
strip_characters([]) ->
    [];
strip_characters([X | Xs]) ->
    case is_not_control_code(X) of 
        true -> [X | strip_characters(Xs)];
        false -> strip_characters(Xs)
    end.

%Pattern matching to determine if a char is a
%control code.
is_not_control_code( C ) when C > 127 -> true;
is_not_control_code( C ) when C < 32; C =:= 127 -> false;
is_not_control_code( _C ) -> true.



