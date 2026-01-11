-module(post_db).
-include_lib("epgsql/include/epgsql.hrl").
-export([get_all_posts/4, create_post/4, update_post/2, delete_post/1, get_post/1]).

get_all_posts(IncAuthor, IncTags, Limit, Offset) ->
    BaseFields = ["p.id", "p.title", "p.content", "p.published", "p.\"authorId\""],
    BaseFrom = ["FROM \"Post\" p"],
    BaseGroupBy = ["GROUP BY p.id"],

    {Fields1, From1, GroupBy1} = case IncAuthor of
        true -> 
            {BaseFields ++ ["u.username", "u.email"], 
             BaseFrom ++ ["LEFT JOIN \"User\" u ON p.\"authorId\" = u.id"],
             BaseGroupBy ++ ["u.id"]};
        false -> 
            {BaseFields, BaseFrom, BaseGroupBy}
    end,

    {Fields2, From2, GroupBy2} = case IncTags of
        true ->
            TagField = "COALESCE(json_agg(json_build_object('id', t.id, 'name', t.name)) FILTER (WHERE t.id IS NOT NULL), '[]') as tags",
            {Fields1 ++ [TagField],
             From1 ++ ["LEFT JOIN \"_PostTags\" pt ON p.id = pt.\"A\"", 
                       "LEFT JOIN \"Tag\" t ON pt.\"B\" = t.id"],
             GroupBy1};
        false ->
            {Fields1, From1, GroupBy1}
    end,

    QueryString = lists:flatten(io_lib:format("SELECT ~s ~s ~s LIMIT $1 OFFSET $2", [
        string:join(Fields2, ", "),
        string:join(From2, " "),
        string:join(GroupBy2, ", ")
    ])),

    {ok, Columns, Rows} = db_worker:query(QueryString, [Limit, Offset]),

    [dynamic_row_map(Columns, Row) || Row <- Rows].

get_post(Id) ->
    Res = db_worker:query("SELECT id, title, content, published, \"authorId\" FROM \"Post\" WHERE id = $1", [Id]),
    case Res of
        {ok, _, [Row]} -> {ok, row_to_map(Row)};
        _ -> {error, not_found}
    end.

create_post(Title, Content, AuthorId, TagIds) ->
    try
        db_worker:transaction(fun(Conn) ->
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
    end.

update_post(Id, Updates) ->
    case db_worker:query("SELECT title, content FROM \"Post\" WHERE id = $1", [Id]) of
        {ok, _, [{OldTitle, OldContent}]} ->
            NewTitle = maps:get(<<"title">>, Updates, OldTitle),
            NewContent = maps:get(<<"content">>, Updates, OldContent),
            
            {ok, _, _, [Row]} = db_worker:query(
                "UPDATE \"Post\" SET title = $1, content = $2 WHERE id = $3 RETURNING id, title, content, published, \"authorId\"",
                [NewTitle, NewContent, Id]),
            {ok, row_to_map(Row)};
        _ ->
            {error, not_found}
    end.

delete_post(Id) ->
    {ok, Count} = db_worker:query("DELETE FROM \"Post\" WHERE id = $1", [Id]),
    case Count of 1 -> ok; 0 -> {error, not_found} end.

dynamic_row_map(Columns, RowTuple) ->
    RowList = tuple_to_list(RowTuple),
    ColNames = [C#column.name || C <- Columns], 
    Map = maps:from_list(lists:zip(ColNames, RowList)),
    case maps:is_key(<<"tags">>, Map) of
        true -> 
            TagsBin = maps:get(<<"tags">>, Map),
            Map#{<<"tags">> => jsx:decode(TagsBin)};
        false -> Map
    end.

row_to_map({Id, Title, Content, Pub, AId}) ->
    #{<<"id">> => Id, <<"title">> => Title, <<"content">> => Content, <<"published">> => Pub, <<"authorId">> => AId}.