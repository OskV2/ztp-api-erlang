-module(post_db).
%% 1. WAŻNE: Importujemy nagłówki epgsql, żeby widzieć rekord #column{}
-include_lib("epgsql/include/epgsql.hrl").

-export([get_all_posts/4, create_post/4, update_post/2, delete_post/1, get_post/1]).

%% --- GET (Pobieranie z filtrowaniem + Paginacja) ---
get_all_posts(IncAuthor, IncTags, Limit, Offset) ->
    %% 1. Baza zapytania
    BaseFields = ["p.id", "p.title", "p.content", "p.published", "p.\"authorId\""],
    BaseFrom = ["FROM \"Post\" p"],
    BaseGroupBy = ["GROUP BY p.id"],

    %% 2. Logika dla AUTORA
    {Fields1, From1, GroupBy1} = case IncAuthor of
        true -> 
            {BaseFields ++ ["u.username", "u.email"], 
             BaseFrom ++ ["LEFT JOIN \"User\" u ON p.\"authorId\" = u.id"],
             BaseGroupBy ++ ["u.id"]};
        false -> 
            {BaseFields, BaseFrom, BaseGroupBy}
    end,

    %% 3. Logika dla TAGÓW
    {Fields2, From2, GroupBy2} = case IncTags of
        true ->
            %% Zauważ zmianę na pt."A" i pt."B" (dla Prismy)
            TagField = "COALESCE(json_agg(json_build_object('id', t.id, 'name', t.name)) FILTER (WHERE t.id IS NOT NULL), '[]') as tags",
            {Fields1 ++ [TagField],
             From1 ++ ["LEFT JOIN \"_PostTags\" pt ON p.id = pt.\"A\"", 
                       "LEFT JOIN \"Tag\" t ON pt.\"B\" = t.id"],
             GroupBy1};
        false ->
            {Fields1, From1, GroupBy1}
    end,

    %% 4. Sklejanie zapytania
    QueryString = lists:flatten(io_lib:format("SELECT ~s ~s ~s LIMIT $1 OFFSET $2", [
        string:join(Fields2, ", "),
        string:join(From2, " "),
        string:join(GroupBy2, ", ")
    ])),

    %% 5. Wykonanie zapytania
    C = db_worker:get_connection(),
    {ok, Columns, Rows} = epgsql:equery(C, QueryString, [Limit, Offset]),
    epgsql:close(C),

    %% Tutaj wywołujemy funkcję, której Ci brakowało
    [dynamic_row_map(Columns, Row) || Row <- Rows].

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
            {ok, _, _, [{PostId, Pub, AuthId}]} = epgsql:equery(Conn, 
                "INSERT INTO \"Post\" (title, content, \"authorId\", published) VALUES ($1, $2, $3, true) RETURNING id, published, \"authorId\"", 
                [Title, Content, AuthorId]),
            
            lists:foreach(fun(TagId) ->
                epgsql:equery(Conn, "INSERT INTO \"_PostTags\" (\"A\", \"B\") VALUES ($1, $2)", [PostId, TagId])
            end, TagIds),

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

%% 2. NOWOŚĆ: To jest funkcja, której brakowało!
dynamic_row_map(Columns, RowTuple) ->
    RowList = tuple_to_list(RowTuple),
    %% Columns to lista rekordów #column{} z epgsql
    ColNames = [C#column.name || C <- Columns], 
    
    %% Tworzymy mapę {<<"nazwa">>, Wartość}
    Map = maps:from_list(lists:zip(ColNames, RowList)),
    
    %% Dekodujemy JSON dla tagów, jeśli istnieje
    case maps:is_key(<<"tags">>, Map) of
        true -> 
            TagsBin = maps:get(<<"tags">>, Map),
            Map#{<<"tags">> => jsx:decode(TagsBin)};
        false -> Map
    end.

%% Stary helper używany przez get_post/1 i update_post/2
row_to_map({Id, Title, Content, Pub, AId}) ->
    #{<<"id">> => Id, <<"title">> => Title, <<"content">> => Content, <<"published">> => Pub, <<"authorId">> => AId}.

%% Usunąłem stare, nieużywane helpery (run_query, row_to_map_with_tags, etc.)