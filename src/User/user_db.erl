-module(user_db).
-export([get_all_users/1, create_user/2, update_user/2, delete_user/1]).

%% Wersja bez profilu (profile=false lub brak parametru)
get_all_users(false) ->
    C = db_worker:get_connection(),
    {ok, _, Rows} = epgsql:equery(C, "SELECT id, username, email FROM \"User\"", []),
    epgsql:close(C),
    [row_to_map(Row) || Row <- Rows];

%% Wersja z profilem (profile=true) -> Używamy LEFT JOIN
get_all_users(true) ->
    C = db_worker:get_connection(),
    %% Pobieramy dane usera (u) oraz profilu (p)
    Query = "SELECT u.id, u.username, u.email, p.bio, p.website 
             FROM \"User\" u 
             LEFT JOIN \"UserProfile\" p ON u.id = p.\"userId\"",
    
    {ok, _, Rows} = epgsql:equery(C, Query, []),
    epgsql:close(C),
    
    %% Mapujemy wyniki używając nowej funkcji pomocniczej
    [row_to_map_with_profile(Row) || Row <- Rows].

%% Tworzenie usera z profilem (transakcja)
create_user(Username, Email) ->
    C = db_worker:get_connection(),
    try
        epgsql:with_transaction(C, fun(Conn) ->
            %% 1. Insert User
            {ok, _, _, [UserRow]} = epgsql:equery(Conn,
                "INSERT INTO \"User\" (username, email) VALUES ($1, $2) RETURNING id, username, email",
                [Username, Email]),
            
            %% Wyciągamy ID z wiersza, żeby przekazać do profilu
            {UserId, _, _} = UserRow,

            %% 2. Insert Profile (w drugim module)
            Profile = user_profile_db:create_default_profile(Conn, UserId),
            
            %% 3. Łączymy mapę Usera z mapą Profilu
            UserMap = row_to_map(UserRow),
            maps:put(<<"profile">>, Profile, UserMap)
        end)
    after
        epgsql:close(C)
    end.

%% Aktualizacja usera
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

%% Usuwanie usera
delete_user(Id) ->
    C = db_worker:get_connection(),
    {ok, Count} = epgsql:equery(C, "DELETE FROM \"User\" WHERE id = $1", [Id]),
    epgsql:close(C),
    case Count of 
        1 -> ok; 
        0 -> {error, not_found} 
    end.

%% --- Funkcje pomocnicze ---

%% Zamienia krotkę (tuple) z bazy na mapę
row_to_map({Id, Username, Email}) ->
    #{
        <<"id">> => Id, 
        <<"username">> => Username, 
        <<"email">> => Email
    }.

%% Nowy helper (dla true)
row_to_map_with_profile({Id, Username, Email, Bio, Website}) ->
    #{
        <<"id">> => Id, 
        <<"username">> => Username, 
        <<"email">> => Email,
        %% Zagnieżdżamy obiekt profile, tak jak robi to Prisma
        <<"profile">> => #{
            <<"bio">> => Bio,
            <<"website">> => Website
        }
    }.