-module(benchmarks).
-compile(export_all).

%Runs benchmarks on a test file for standard map vs parallel map.
test_map(Runs) ->
    {_, BJSON} = file:read_file("json.txt"),
    JSON = binary_to_list(BJSON),
    {_, DecodedResponse} = mochijson2:decode(JSON),
    Return = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,DecodedResponse)),
    Func = fun(Y) -> proplists:get_value(<<"text">>, Y) end,
    
    {PTime, _} = timer:tc(putils, p_map, [Func,Return]),
    {STime, _} = timer:tc(lists, map, [Func, Return]),    

    {PTime, PThousand, STime, SThousand}.





%Runs benchmarks on sequential http requests vs parallel http requests.
test_search({Consumer, AccessParams}, Query) ->
    {PTime, _} = timer:tc(benchmarks, search_spawner, [{Consumer, AccessParams}, Query, 10]),
    {STime, _} = timer:tc(benchmarks, s_search_spawner,[{Consumer, AccessParams}, Query, 10]),
    {PTime, STime}.

%Helper function to spawn the processes, corresponds to similar code in twitter.erl
search_spawner({Consumer, AccessParams}, Query, 0) ->
    [];
search_spawner({Consumer, AccessParams}, Query, X) ->
    S = self(),
    Pid1 = spawn(fun () -> twitter:parallel_search_supervisor(S, {Consumer, AccessParams}, Query) end),
    Pid2 = spawn(fun () -> twitter:parallel_search_supervisor(S, {Consumer, AccessParams}, Query) end),
    Pid3 = spawn(fun () -> twitter:parallel_search_supervisor(S, {Consumer, AccessParams}, Query) end),
    Pid4 = spawn(fun () -> twitter:parallel_search_supervisor(S, {Consumer, AccessParams}, Query) end),
    Pid5 = spawn(fun () -> twitter:parallel_search_supervisor(S, {Consumer, AccessParams}, Query) end),
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
    Response1 ++ Response2 ++ Response3 ++ Response4 ++ Response5
    ++ search_spawner({Consumer, AccessParams}, Query, X).

%Helper function to do http requests sequentially.
s_search_spawner({Consumer, AccessParams}, Query) ->
    {_, DecodedResponse1} = mochijson2:decode(twitter:search({Consumer, AccessParams}, Query)),
    UsefulDataResponse1 = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,
    DecodedResponse1)),
    {_, DecodedResponse2} = mochijson2:decode(twitter:search({Consumer, AccessParams}, Query)),
    UsefulDataResponse2 = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,
    DecodedResponse2)),
    {_, DecodedResponse3} = mochijson2:decode(twitter:search({Consumer, AccessParams}, Query)),
    UsefulDataResponse3 = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,
    DecodedResponse3)),
    {_, DecodedResponse4} = mochijson2:decode(twitter:search({Consumer, AccessParams}, Query)),
    UsefulDataResponse4 = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,
    DecodedResponse4)),
    {_, DecodedResponse5} = mochijson2:decode(twitter:search({Consumer, AccessParams}, Query)),
    UsefulDataResponse5 = twitter:strip_useless_tweets_data(proplists:get_value(<<"statuses">>,
    DecodedResponse5)).