-module(mse_chat_room_instance_sup).
-behaviour(supervisor).
-define(MAXT, 3600).
-define(MAXR, 4).
-define(SERVER, ?MODULE).

-export([init/1, create_new_room/1, start_link/0]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_new_room(RoomName) ->
  supervisor:start_child(?SERVER, [RoomName]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
  Room = {mse_chat_room_instance, {mse_chat_room_instance, start_link, []},
            temporary, 2000, worker, [mse_chat_room_instance]},
  Children = [Room],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

