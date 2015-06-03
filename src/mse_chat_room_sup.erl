-module(mse_chat_room_sup).
-behaviour(supervisor).
-define(MAXT, 3600).
-define(MAXR, 4).
-define(SERVER, ?MODULE).
-export([init/1, start_link/0]).

%%% Room supervisor, supervises the room registry and the room instance supervisor


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
  RoomManager = {
    mse_chat_room_manager,
    {mse_chat_room_manager, start_link, []},
    permanent, 2000, worker, [mse_chat_room_manager]
  },

  RoomInstanceSupervisor = {
    mse_chat_room_instance_sup,
    {mse_chat_room_instance_sup, start_link, []},
    permanent, 2000, supervisor, [mse_chat_room_instance_sup]
  },

  RestartStrategyTuple = {one_for_one, ?MAXT, ?MAXR},
  {ok, {RestartStrategyTuple, [RoomManager, RoomInstanceSupervisor]}}.

