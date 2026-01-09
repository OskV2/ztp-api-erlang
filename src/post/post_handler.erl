-module(post_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Id = cowboy_req:binding(id, Req0),
    handle(Method, Id, Req0, State).

%% GET /api/v1/post (parametry query)
handle(<<"GET">>, undefined, Req, State) ->
    Qs = cowboy_req:parse_qs(Req),
    
    %% Sprawdzamy flagi ?author=true i ?tags=true
    IncAuthor = is_true(proplists:get_value(<<"author">>, Qs)),
    IncTags = is_true(proplists:get_value(<<"tags">>, Qs)),

    Posts = post_db:get_all_posts(IncAuthor, IncTags),
    reply_json(200, Posts, Req, State);

%% GET /api/v1/post/:id (Pojedynczy)
handle(<<"GET">>, IdBin, Req, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    case post_db:get_post(Id) of
        {ok, Post} -> reply_json(200, Post, Req, State);
        {error, not_found} -> reply_error(404, Req, State)
    end;

%% POST /api/v1/post (Create)
handle(<<"POST">>, undefined, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    
    Title = maps:get(<<"title">>, Data),
    Content = maps:get(<<"content">>, Data),
    AuthorId = maps:get(<<"authorId">>, Data),
    %% Tagi są opcjonalne, domyślnie pusta lista
    TagIds = maps:get(<<"tags">>, Data, []), 

    case post_db:create_post(Title, Content, AuthorId, TagIds) of
        {error, author_or_tag_not_found} -> 
            cowboy_req:reply(400, #{}, <<"Author or Tag does not exist">>, Req1),
            {ok, Req1, State};
        {error, _} -> 
            reply_error(500, Req1, State);
        NewPost -> 
            reply_json(201, NewPost, Req1, State)
    end;

%% PATCH /api/v1/post/:id
handle(<<"PATCH">>, IdBin, Req0, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Updates = jsx:decode(Body, [return_maps]),
    
    case post_db:update_post(Id, Updates) of
        {ok, Updated} -> reply_json(200, Updated, Req1, State);
        {error, not_found} -> reply_error(404, Req1, State)
    end;

%% DELETE /api/v1/post/:id
handle(<<"DELETE">>, IdBin, Req, State) when IdBin /= undefined ->
    Id = binary_to_integer(IdBin),
    case post_db:delete_post(Id) of
        ok -> 
            Req1 = cowboy_req:reply(204, Req),
            {ok, Req1, State};
        {error, not_found} -> reply_error(404, Req, State)
    end;

handle(_, _, Req, State) ->
    cowboy_req:reply(405, Req),
    {ok, Req, State}.

%% Helpers
is_true(<<"true">>) -> true;
is_true(_) -> false.

reply_json(Status, Data, Req, State) ->
    Body = jsx:encode(Data),
    Req1 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {ok, Req1, State}.

reply_error(Status, Req, State) ->
    cowboy_req:reply(Status, Req),
    {ok, Req, State}.