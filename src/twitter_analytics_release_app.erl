-module(twitter_analytics_release_app).
-author("Samuel Cox").
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% Hooks up various handler modules to urls.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, twitter_analytics_release, "analytics.html"}},
			{"/stream", cowboy_static, {priv_file, twitter_analytics_release, "stream.html"}},
			{"/websocket", websocket_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, twitter_analytics_release, "static"}},
			{"/api/aggregate", aggregate_handler, []}			
			]}
		]),
		{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
			env => #{dispatch => Dispatch},
			request_timeout => 99999

		}),
		twitter_analytics_release_sup:start_link().

stop(_State) ->
	ok.
