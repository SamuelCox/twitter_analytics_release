-module(ets_helpers).
-author("Samuel Cox").
-compile(export_all).



new_tweets_table() ->
    ets:new(tweets_table, [public, duplicate_bag, named_table]).

store_tweets_list_in_ets(List, Name) ->    
  catch ets:insert(tweets_table, {Name, List}).

store_json_decode(Name, List) ->
    catch ets:insert(tweets_table, {Name, List}).

get_tweets(Name) ->
    ets:lookup(tweets_table,Name).