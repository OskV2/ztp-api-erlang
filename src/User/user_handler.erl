-module(user_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    %% Wyciągamy ID ze ścieżki (jeśli istnieje)
    %% path_info dla /api/v1/user/5 zwróci [<<"5">>]
    PathInfo = cowboy_req:path_info(Req0),
    handle(Method, PathInfo, Req0, State).

%% GET /api/v1/user
handle(<<"GET">>, _, Req, State) ->
    Users = user_db:get_all_users(),
    reply_json(200, Users, Req, State);

%% POST /api/v1/user
handle(<<"POST">>, _, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Data),
    Email = maps:get(<<"email">>, Data),
    {ok, NewUser} = user_db:create_user(Username, Email),
    reply_json(201, NewUser, Req1, State);

%% PATCH /api/v1/user/{id}
handle(<<"PATCH">>, [IdBin], Req0, State) ->
    Id = binary_to_integer(IdBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Updates = jsx:decode(Body, [return_maps]),
    case user_db:update_user(Id, Updates) of
        {ok, User} -> reply_json(200, User, Req1, State);
        {error, not_found} -> reply_error(404, Req1, State)
    end;

%% DELETE /api/v1/user/{id}
handle(<<"DELETE">>, [IdBin], Req, State) ->
    Id = binary_to_integer(IdBin),
    case user_db:delete_user(Id) of
        ok -> cowboy_req:reply(204, Req), {ok, Req, State};
        {error, not_found} -> reply_error(404, Req, State)
    end;

handle(_, _, Req, State) ->
    cowboy_req:reply(405, Req),
    {ok, Req, State}.

%% Funkcje pomocnicze
reply_json(Status, Data, Req, State) ->
    Body = jsx:encode(Data),
    Req1 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {ok, Req1, State}.

reply_error(Status, Req, State) ->
    cowboy_req:reply(Status, Req),
    {ok, Req, State}.