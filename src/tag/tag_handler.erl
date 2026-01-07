-module(tag_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    %% Wyciągamy ID (jeśli jesteśmy na ścieżce /tags/:id)
    Id = cowboy_req:binding(id, Req0),
    handle(Method, Id, Req0, State).

%% GET /api/v1/tags (Wszystkie)
handle(<<"GET">>, undefined, Req, State) ->
    Tags = tag_db:get_all_tags(),
    reply_json(200, Tags, Req, State);

%% GET /api/v1/tags/:id (Jeden)
handle(<<"GET">>, IdBin, Req, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    case tag_db:get_tag(Id) of
        {ok, Tag} -> reply_json(200, Tag, Req, State);
        {error, not_found} -> reply_error(404, Req, State)
    end;

%% POST /api/v1/tags (Tworzenie)
handle(<<"POST">>, undefined, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    Name = maps:get(<<"name">>, Data),
    
    {ok, NewTag} = tag_db:create_tag(Name),
    reply_json(201, NewTag, Req1, State);

%% PATCH /api/v1/tags/:id (Edycja)
handle(<<"PATCH">>, IdBin, Req0, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Updates = jsx:decode(Body, [return_maps]),
    
    case tag_db:update_tag(Id, Updates) of
        {ok, UpdatedTag} -> reply_json(200, UpdatedTag, Req1, State);
        {error, not_found} -> reply_error(404, Req1, State)
    end;

%% DELETE /api/v1/tags/:id (Usuwanie)
handle(<<"DELETE">>, IdBin, Req, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    case tag_db:delete_tag(Id) of
        ok -> 
            Req1 = cowboy_req:reply(204, Req),
            {ok, Req1, State};
        {error, not_found} -> reply_error(404, Req, State)
    end;

handle(_, _, Req, State) ->
    cowboy_req:reply(405, Req),
    {ok, Req, State}.

%% Helpers
reply_json(Status, Data, Req, State) ->
    Body = jsx:encode(Data),
    Req1 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {ok, Req1, State}.

reply_error(Status, Req, State) ->
    cowboy_req:reply(Status, Req),
    {ok, Req, State}.