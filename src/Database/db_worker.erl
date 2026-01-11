-module(db_worker).
-behavior(gen_server).
-behavior(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([query/1, query/2, transaction/1]).

-include_lib("epgsql/include/epgsql.hrl").
%%  Zapytanie bez parametrów (np. SELECT * FROM table)
query(Sql) ->
    query(Sql, []).

%% Zapytanie z parametrami (np. WHERE id = $1)
query(Sql, Params) ->
    poolboy:transaction(db_pool, fun(Worker) ->
        gen_server:call(Worker, {equery, Sql, Params})
    end).

%% Obsługa transakcji np. dla create_post
transaction(Fun) ->
    poolboy:transaction(db_pool, fun(Worker) ->
        gen_server:call(Worker, {transaction, Fun})
    end).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    {ok, C} = epgsql:connect(#{
        host => "localhost",
        username => "postgres",
        password => "root", 
        database => "ztp_project",     
        timeout => 5000
    }),
    {ok, C}.

%% Obsługa zapytania wewnątrz workera
handle_call({equery, Sql, Params}, _From, Conn) ->
    Result = epgsql:equery(Conn, Sql, Params),
    {reply, Result, Conn};

%% Obsługa transakcji wewnątrz workera
handle_call({transaction, Fun}, _From, Conn) ->
    Result = epgsql:with_transaction(Conn, Fun),
    {reply, Result, Conn};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Conn) ->
    epgsql:close(Conn),
    ok.