-module(twitter_analytics_release_sup).
-author("Samuel Cox").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% Initialises some libaries needed by the application.
start_link() ->
	inets:start(),
	ssl:start(),
	twitter:init(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
