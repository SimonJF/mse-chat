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

create_new_room(Args) ->
  supervisor:start_child(?SERVER, [Args]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
  Room = {mse_chat_room, {mse_chat_room, start_link, []},
            temporary, 2000, worker, [mse_chat_room]},
  Children = [Room],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
