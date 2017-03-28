# Twitter_Analytics_ResearchProject
A research project designed to both analyse the feasibility of, and practically apply, massively parallel analytics in Erlang.

# Instructions
To run this program, simply clone the repo into a unix environment.
Type make run, and hit enter.
Then, in the erl shell, type ets:insert(tweets_table, {"token", C}).
Where C is a variable containing the oauth token supplied to you by twitter. For the
research project, this is provided in token.txt of the corpus.
Then go to machine-name:8080 and you will have reached the web-app. Things should be
relatively intuitive from there, but on the search analytics page, please be patient!
It will analyse 14,000 tweets and so can take a while. It is doing lots of work in the
background.