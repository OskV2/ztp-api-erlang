%%%-------------------------------------------------------------------
%% @doc ztp_api_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(ztp_api_erlang_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/user/[...]", user_handler, []}
            %% dodaj inne ścieżki tutaj
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    %% To musi być ostatnia linia i musi zwracać {ok, Pid}
    ztp_api_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions