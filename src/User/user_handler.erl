-module(user_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    %% Używamy binding - to zwraca pojedyncze binary (np. <<"5">>) lub undefined
    Id = cowboy_req:binding(id, Req0),
    handle(Method, Id, Req0, State).

%% GET /api/v1/user?profile=true
handle(<<"GET">>, undefined, Req, State) ->
    %% 1. Parsujemy Query String (zwraca listę krotek [{<<"key">>, <<"val">>}, ...])
    Qs = cowboy_req:parse_qs(Req),
    
    %% 2. Sprawdzamy wartość klucza "profile"
    %% proplists:get_value szuka klucza w liście. Jeśli nie znajdzie, zwraca "false" (trzeci argument)
    IncludeProfile = case proplists:get_value(<<"profile">>, Qs, <<"false">>) of
        <<"true">> -> true;
        _ -> false
    end,

    %% 3. Przekazujemy flagę do bazy danych
    Users = user_db:get_all_users(IncludeProfile),
    
    reply_json(200, Users, Req, State);

%% POST /api/v1/user (bez ID)
handle(<<"POST">>, undefined, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Data),
    Email = maps:get(<<"email">>, Data),
    
    %% ZMIANA 2: Usunięto "{ok, }". Teraz pasuje do tego, co zwraca user_db (mapa).
    NewUser = user_db:create_user(Username, Email),
    
    %% --- DEBUGOWANIE START ---
    io:format("~n[DEBUG] Dane do JSONa: ~p~n", [NewUser]),
    %% --- DEBUGOWANIE END ---

    reply_json(201, NewUser, Req1, State);

%% PATCH /api/v1/user/{id}
%% ZMIANA 3: Dopasowanie do zmiennej Id (binary), a nie listy [IdBin]
handle(<<"PATCH">>, IdBin, Req0, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Updates = jsx:decode(Body, [return_maps]),
    case user_db:update_user(Id, Updates) of
        {ok, User} -> reply_json(200, User, Req1, State);
        {error, not_found} -> reply_error(404, Req1, State)
    end;

%% DELETE /api/v1/user/{id}
handle(<<"DELETE">>, IdBin, Req, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    case user_db:delete_user(Id) of
        ok -> 
            Req1 = cowboy_req:reply(204, Req),
            {ok, Req1, State};
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