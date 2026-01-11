%%%-------------------------------------------------------------------
%% @doc ztp_api_erlang public API
%% @end
%%%-------------------------------------------------------------------

%%  HOW TO RUN:
%%  rebar3 compile && rebar3 shell

-module(ztp_api_erlang_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            % user
            {"/api/v1/user", user_handler, []},
            {"/api/v1/user/:id", user_handler, []},
            
            % user profile
            {"/api/v1/user-profile/:id", user_profile_handler, []},
            
            % tag
            {"/api/v1/tag", tag_handler, []},
            {"/api/v1/tag/:id", tag_handler, []},

            % post
            {"/api/v1/post", post_handler, []},
            {"/api/v1/post/:id", post_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ztp_api_erlang_sup:start_link().

stop(_State) ->
    ok.