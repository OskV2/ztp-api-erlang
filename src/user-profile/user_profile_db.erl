-module(user_profile_db).
-export([get_profile/1, update_profile/2, create_default_profile/2]).

%% Wywoływane przez user_db wewnątrz transakcji
create_default_profile(Conn, UserId) ->
    Bio = <<"Hello! I'm new user!">>,
    Website = null,
    {ok, _} = epgsql:equery(Conn,
        "INSERT INTO \"UserProfile\" (\"userId\", bio, website) VALUES ($1, $2, $3)",
        [UserId, Bio, Website]),
    #{<<"bio">> => Bio, <<"website">> => Website}.

get_profile(UserId) ->
    C = db_worker:get_connection(),
    Result = epgsql:equery(C, "SELECT bio, website FROM \"UserProfile\" WHERE \"userId\" = $1", [UserId]),
    epgsql:close(C),
    case Result of
        {ok, _, [{Bio, Website}]} -> {ok, #{<<"bio">> => Bio, <<"website">> => Website}};
        _ -> {error, not_found}
    end.

update_profile(UserId, Updates) ->
    C = db_worker:get_connection(),
    Bio = maps:get(<<"bio">>, Updates, null),
    Website = maps:get(<<"website">>, Updates, null),
    Result = epgsql:equery(C, 
        "UPDATE \"UserProfile\" SET bio = $1, website = $2 WHERE \"userId\" = $3 RETURNING bio, website",
        [Bio, Website, UserId]),
    epgsql:close(C),
    case Result of
        {ok, 1, _, [{NewBio, NewWeb}]} -> {ok, #{<<"bio">> => NewBio, <<"website">> => NewWeb}};
        _ -> {error, not_found}
    end.