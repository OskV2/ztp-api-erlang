-module(post_db).
-export([get_all_posts/2, create_post/4, update_post/2, delete_post/1, get_post/1]).

%% --- GET (Pobieranie z filtrowaniem) ---

%% 1. Tylko same posty (bez autora, bez tagów)
get_all_posts(false, false) ->
    run_query("SELECT id, title, content, published, \"authorId\" FROM \"Post\"", [], fun row_to_map/1);

%% 2. Posty z Autorem (JOIN User)
get_all_posts(true, false) ->
    Query = "SELECT p.id, p.title, p.content, p.published, p.\"authorId\", u.username, u.email 
             FROM \"Post\" p 
             LEFT JOIN \"User\" u ON p.\"authorId\" = u.id",
    run_query(Query, [], fun row_to_map_with_author/1);

%% 3. Posty z Tagami (JOIN Tags + GROUP BY + ARRAY_AGG)
get_all_posts(false, true) ->
    %% ZMIANA: Używamy A i B z Prismy
    Query = "SELECT p.id, p.title, p.content, p.published, p.\"authorId\", 
             COALESCE(json_agg(json_build_object('id', t.id, 'name', t.name)) FILTER (WHERE t.id IS NOT NULL), '[]') as tags
             FROM \"Post\" p
             LEFT JOIN \"_PostTags\" pt ON p.id = pt.\"A\" 
             LEFT JOIN \"Tag\" t ON pt.\"B\" = t.id
             GROUP BY p.id",
    run_query(Query, [], fun row_to_map_with_tags/1);

%% 4. Posty z Autorem i Tagami (Full opcja)
get_all_posts(true, true) ->
    %% ZMIANA: Używamy A i B z Prismy
    Query = "SELECT p.id, p.title, p.content, p.published, p.\"authorId\", u.username, u.email,
             COALESCE(json_agg(json_build_object('id', t.id, 'name', t.name)) FILTER (WHERE t.id IS NOT NULL), '[]') as tags
             FROM \"Post\" p
             LEFT JOIN \"User\" u ON p.\"authorId\" = u.id
             LEFT JOIN \"_PostTags\" pt ON p.id = pt.\"A\"
             LEFT JOIN \"Tag\" t ON pt.\"B\" = t.id
             GROUP BY p.id, u.id",
    run_query(Query, [], fun row_to_map_full/1).

%% --- POZOSTAŁE CRUD ---

get_post(Id) ->
    C = db_worker:get_connection(),
    Res = epgsql:equery(C, "SELECT id, title, content, published, \"authorId\" FROM \"Post\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Res of
        {ok, _, [Row]} -> {ok, row_to_map(Row)};
        _ -> {error, not_found}
    end.

create_post(Title, Content, AuthorId, TagIds) ->
    C = db_worker:get_connection(),
    try
        epgsql:with_transaction(C, fun(Conn) ->
            %% 1. Tworzymy Post
            {ok, _, _, [{PostId, Pub, AuthId}]} = epgsql:equery(Conn, 
                "INSERT INTO \"Post\" (title, content, \"authorId\", published) VALUES ($1, $2, $3, true) RETURNING id, published, \"authorId\"", 
                [Title, Content, AuthorId]),
            
            %% 2. Dodajemy Tagi (Prisma style: A=PostId, B=TagId)
            lists:foreach(fun(TagId) ->
                epgsql:equery(Conn, "INSERT INTO \"_PostTags\" (\"A\", \"B\") VALUES ($1, $2)", [PostId, TagId])
            end, TagIds),

            %% Zwracamy pełną mapę (bez kropek!)
            #{
                <<"id">> => PostId, 
                <<"title">> => Title, 
                <<"content">> => Content, 
                <<"published">> => Pub,
                <<"authorId">> => AuthId, 
                <<"tags">> => TagIds
            }
        end)
    catch
        %% Obsługa błędu FK (np. brak autora lub tagu)
        error:{error, {error, error, _, _, <<"23503">>, _}} -> {error, author_or_tag_not_found};
        _:_ -> {error, server_error}
    after
        epgsql:close(C)
    end.

update_post(Id, Updates) ->
    C = db_worker:get_connection(),
    case epgsql:equery(C, "SELECT title, content FROM \"Post\" WHERE id = $1", [Id]) of
        {ok, _, [{OldTitle, OldContent}]} ->
            NewTitle = maps:get(<<"title">>, Updates, OldTitle),
            NewContent = maps:get(<<"content">>, Updates, OldContent),
            
            {ok, _, _, [Row]} = epgsql:equery(C, 
                "UPDATE \"Post\" SET title = $1, content = $2 WHERE id = $3 RETURNING id, title, content, published, \"authorId\"",
                [NewTitle, NewContent, Id]),
            epgsql:close(C),
            {ok, row_to_map(Row)};
        _ ->
            epgsql:close(C),
            {error, not_found}
    end.

delete_post(Id) ->
    C = db_worker:get_connection(),
    {ok, Count} = epgsql:equery(C, "DELETE FROM \"Post\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Count of 1 -> ok; 0 -> {error, not_found} end.

%% --- HELPERS ---

run_query(Query, Args, Mapper) ->
    C = db_worker:get_connection(),
    {ok, _, Rows} = epgsql:equery(C, Query, Args),
    epgsql:close(C),
    [Mapper(Row) || Row <- Rows].

row_to_map({Id, Title, Content, Pub, AId}) ->
    #{<<"id">> => Id, <<"title">> => Title, <<"content">> => Content, <<"published">> => Pub, <<"authorId">> => AId}.

row_to_map_with_author({Id, Title, Content, Pub, AId, UName, UEmail}) ->
    Base = row_to_map({Id, Title, Content, Pub, AId}),
    Base#{<<"author">> => #{<<"username">> => UName, <<"email">> => UEmail}}.

row_to_map_with_tags({Id, Title, Content, Pub, AId, TagsJson}) ->
    Base = row_to_map({Id, Title, Content, Pub, AId}),
    Base#{<<"tags">> => jsx:decode(TagsJson)}.

row_to_map_full({Id, Title, Content, Pub, AId, UName, UEmail, TagsJson}) ->
    Base = row_to_map_with_author({Id, Title, Content, Pub, AId, UName, UEmail}),
    Base#{<<"tags">> => jsx:decode(TagsJson)}.