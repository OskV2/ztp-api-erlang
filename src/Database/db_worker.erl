-module(db_worker).
-export([get_connection/0]).

get_connection() ->
    %% Dane do bazy - w wersji docelowej powinny być w sys.config
    Config = #{
        host => "localhost",
        port => 5432,
        username => "postgres",
        password => "root",
        database => "ztp_project",
        timeout => 5000
    },
    {ok, C} = epgsql:connect(Config),
    C.