-module(user_db).
-export([get_all_users/0, create_user/2, update_user/2, delete_user/1]).

get_all_users() ->
    C = db_worker:get_connection(),
    {ok, _, Rows} = epgsql:equery(C, "SELECT id, username, email FROM \"User\"", []),
    epgsql:close(C),
    [ #{<<"id">> => Id, <<"username">> => Un, <<"email">> => Em} || {Id, Un, Em} <- Rows ].

create_user(Username, Email) ->
    C = db_worker:get_connection(),
    Result = epgsql:equery(C, 
        "INSERT INTO \"User\" (username, email) VALUES ($1, $2) RETURNING id, username, email", 
        [Username, Email]),
    epgsql:close(C),
    case Result of
        {ok, 1, _, [Row]} -> {ok, row_to_map(Row)};
        Error -> Error
    end.

update_user(Id, Updates) ->
    C = db_worker:get_connection(),
    %% W Erlangu budowanie dynamicznych zapytań SQL jest nieco trudniejsze niż w Prismie.
    %% Na potrzeby projektu zróbmy prosty update username:
    NewUsername = maps:get(<<"username">>, Updates),
    Result = epgsql:equery(C, 
        "UPDATE \"User\" SET username = $1 WHERE id = $2 RETURNING id, username, email", 
        [NewUsername, Id]),
    epgsql:close(C),
    case Result of
        {ok, 1, _, [Row]} -> {ok, row_to_map(Row)};
        _ -> {error, not_found}
    end.

delete_user(Id) ->
    C = db_worker:get_connection(),
    {ok, Count} = epgsql:equery(C, "DELETE FROM \"User\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Count of
        1 -> ok;
        0 -> {error, not_found}
    end.

%% Helper do zamiany tupli na mapę
row_to_map({Id, Un, Em}) ->
    #{<<"id">> => Id, <<"username">> => Un, <<"email">> => Em}.