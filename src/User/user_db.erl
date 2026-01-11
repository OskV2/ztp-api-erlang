-module(user_db).
-export([get_all_users/1, create_user/2, update_user/2, delete_user/1]).

%%  profile = false
get_all_users(false) ->
    C = db_worker:get_connection(),
    {ok, _, Rows} = epgsql:equery(C, "SELECT id, username, email FROM \"User\"", []),
    epgsql:close(C),
    [row_to_map(Row) || Row <- Rows];

%%  profile = true
get_all_users(true) ->
    C = db_worker:get_connection(),
    Query = "SELECT u.id, u.username, u.email, p.bio, p.website 
             FROM \"User\" u 
             LEFT JOIN \"UserProfile\" p ON u.id = p.\"userId\"",
    
    {ok, _, Rows} = epgsql:equery(C, Query, []),
    epgsql:close(C),
    
    [row_to_map_with_profile(Row) || Row <- Rows].

create_user(Username, Email) ->
    C = db_worker:get_connection(),
    try
        epgsql:with_transaction(C, fun(Conn) ->
            {ok, _, _, [UserRow]} = epgsql:equery(Conn,
                "INSERT INTO \"User\" (username, email) VALUES ($1, $2) RETURNING id, username, email",
                [Username, Email]),
            
            {UserId, _, _} = UserRow,

            Profile = user_profile_db:create_default_profile(Conn, UserId),
            
            UserMap = row_to_map(UserRow),
            maps:put(<<"profile">>, Profile, UserMap)
        end)
    after
        epgsql:close(C)
    end.

update_user(Id, Updates) ->
    C = db_worker:get_connection(),
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

%  Helpers
row_to_map({Id, Username, Email}) ->
    #{
        <<"id">> => Id, 
        <<"username">> => Username, 
        <<"email">> => Email
    }.

row_to_map_with_profile({Id, Username, Email, Bio, Website}) ->
    #{
        <<"id">> => Id, 
        <<"username">> => Username, 
        <<"email">> => Email,
        <<"profile">> => #{
            <<"bio">> => Bio,
            <<"website">> => Website
        }
    }.