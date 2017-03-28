-module(aggregate_handler).
-author("Samuel Cox").
-compile(export_all).

%Initialises the module,
%called only by Cowboy
init(Req, Opts) ->
    {cowboy_rest, Req, opts}.

% Has to be in here to conform to Cowboy's spec
% lets Cowboy know what content types are returned.
content_types_provided(Req, State) ->
    {[
            {<<"application/json">>, aggregate}
            
    ], Req, State}.

    
% The restful method used for the search page
% in the web app. Just calls into the twitter and
% analytics code.
aggregate(Req, State) ->
    [{_, C}] = ets:lookup(tweets_table, "token"),
    Params = cowboy_req:parse_qs(Req),
    {_, Query} = lists:keyfind(<<"query">>,1, Params),
    io:format("About to search", []),
    case twitter:search_and_aggregate(C, binary_to_list(Query), 28, true) of 
        "Rate Limit reached, wait 15 minutes" -> 
            JSON = jsx:encode([{<<"error">>, <<"Rate limit reached">>}]);
        "Cache does not match Query, please wait 15 minutes" ->
            JSON = jsx:encode([{<<"error">>, <<"Rate Limit reached">>}]);
        Resp ->
            {Good, Bad, Equal} = Resp,
            Total = Good + Bad + Equal,
            io:format("Getting words", []),
            Words = twitter:get_most_common_words(),
            PropList = analytics:trim_map(Words),
            BinaryPropList = lists:map(fun({X, Y}) -> {list_to_binary(X), Y} end, PropList),
            io:format("Getting Trends", []),
            Trends = twitter:find_common_trends(C),
            io:format("Getting extras", []),
            {RtPct, RtCount, FavPct, FavCt} = twitter:get_extra_analytics(),
            io:format("Encoding", []),
            JSON = jsx:encode([{<<"good">>, Good}, {<<"bad">>, Bad}, {<<"equal">>, Equal},
                            {<<"total">>, Total}, {<<"trends">>, Trends},
                            {<<"words">>, BinaryPropList},
                            {<<"rtpct">>, RtPct}, {<<"rtcount">>, RtCount},
                            {<<"favpct">>, FavPct}, {<<"favct">>, FavCt},
                            {<<"error">>, <<"none">>}]),
            io:format("Finished encoding",[])
    end,
    io:format(" About to send back to client", []),
    {JSON, Req, State}.


