-module(tag_db).
-export([get_all_tags/0, get_tag/1, create_tag/1, update_tag/2, delete_tag/1]).

get_all_tags() ->
    C = db_worker:get_connection(),
    {ok, _, Rows} = epgsql:equery(C, "SELECT id, name FROM \"Tag\"", []),
    epgsql:close(C),
    [row_to_map(Row) || Row <- Rows].

get_tag(Id) ->
    C = db_worker:get_connection(),
    Result = epgsql:equery(C, "SELECT id, name FROM \"Tag\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Result of
        {ok, _, [Row]} -> {ok, row_to_map(Row)};
        _ -> {error, not_found}
    end.

create_tag(Name) ->
    C = db_worker:get_connection(),
    Result = epgsql:equery(C, 
        "INSERT INTO \"Tag\" (name) VALUES ($1) RETURNING id, name", 
        [Name]),
    epgsql:close(C),
    case Result of
        {ok, 1, _, [Row]} -> {ok, row_to_map(Row)};
        Error -> Error 
    end.

update_tag(Id, Updates) ->
    C = db_worker:get_connection(),
    NewName = maps:get(<<"name">>, Updates),
    Result = epgsql:equery(C, 
        "UPDATE \"Tag\" SET name = $1 WHERE id = $2 RETURNING id, name", 
        [NewName, Id]),
    epgsql:close(C),
    case Result of
        {ok, 1, _, [Row]} -> {ok, row_to_map(Row)};
        _ -> {error, not_found}
    end.

delete_tag(Id) ->
    C = db_worker:get_connection(),
    {ok, Count} = epgsql:equery(C, "DELETE FROM \"Tag\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Count of 
        1 -> ok; 
        0 -> {error, not_found} 
    end.

%% Helper
row_to_map({Id, Name}) ->
    #{<<"id">> => Id, <<"name">> => Name}.