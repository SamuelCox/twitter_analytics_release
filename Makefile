PROJECT = twitter_analytics_release
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy mochiweb jsx oauth

dep_cowboy_commit = master

dep_mochiweb_commit = master

dep_jsx_commit = master

dep_oauth_commit = master

LOCAL_DEPS = inets

DEP_PLUGINS = cowboy

ERLC_OPTS = -W0

include erlang.mk
