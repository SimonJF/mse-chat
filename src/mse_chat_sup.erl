-module(mse_chat_sup).
-behaviour(supervisor).
-define(MAXT, 4).
-define(MAXR, 4).
-define(PORT, 9002).

-export([init/1, start_link/0]).

%%% Root Supervisor
% Standard one-for-one restart of all components.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
  % Room Supervisor
  RoomSupervisor = {
    mse_chat_room_sup,
    {mse_chat_room_sup, start_link, []},
    permanent, 2000, supervisor, [mse_chat_room_sup]
  },

  ClientSupervisor = {
    mse_client_sup,
    {mse_client_sup, start_link, []},
    permanent, 2000, supervisor, [mse_client_sup]
  },

  TCPServer = {
    mse_chat_tcp_server,
    {mse_chat_tcp_server, start_link, [?PORT]},
    permanent, 2000, worker, [mse_chat_tcp_server]
  },

  RestartStrategyTuple = {one_for_one, ?MAXT, ?MAXR},
  Children = [RoomSupervisor, TCPServer, ClientSupervisor],
  {ok, {RestartStrategyTuple, Children}}.
