-module(mse_client_sup).
-behaviour(supervisor).
-define(MAXT, 3600).
-define(MAXR, 4).


%%% Supervisor of TCP server, client registry,
%%% and client thread supervisor

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    ClientRegistry = {
    mse_client_registry,
    {mse_client_registry, start_link, []},
    permanent, 2000, worker, [mse_client_registry]
  },

  % DB Thread Supervisor
  ClientSup = {
    mse_chat_client_sup,
    {mse_chat_client_sup, start_link, []},
    permanent, 2000, supervisor, [mse_chat_client_sup]
  },

  RestartStrategyTuple = {one_for_all, ?MAXT, ?MAXR},
  Children = [ClientRegistry, ClientSup],
  {ok, {RestartStrategyTuple, Children}}.
