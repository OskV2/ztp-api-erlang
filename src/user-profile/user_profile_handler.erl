-module(user_profile_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    %% ZMIANA: Używamy binding, tak jak w user_handler
    Id = cowboy_req:binding(id, Req0),
    handle(Method, Id, Req0, State).

%% GET /api/v1/user-profile/:id
handle(<<"GET">>, IdBin, Req, State) when IdBin /= undefined ->
    UserId = binary_to_integer(IdBin),
    %% POPRAWKA: Wywołujemy moduł user_profile_db!
    case user_profile_db:get_profile(UserId) of
        {ok, Profile} -> reply_json(200, Profile, Req, State);
        {error, not_found} -> reply_error(404, Req, State)
    end;

%% PATCH /api/v1/user-profile/:id
handle(<<"PATCH">>, IdBin, Req0, State) when IdBin /= undefined ->
    UserId = binary_to_integer(IdBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Updates = jsx:decode(Body, [return_maps]),
    %% POPRAWKA: Wywołujemy moduł user_profile_db!
    case user_profile_db:update_profile(UserId, Updates) of
        {ok, Profile} -> reply_json(200, Profile, Req1, State);
        {error, not_found} -> reply_error(404, Req1, State)
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