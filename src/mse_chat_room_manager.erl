-module(mse_chat_room_manager).
-behaviour(gen_server).

-export([init/1, start_link/0, handle_call/3, handle_cast/2, code_change/3,
         handle_info/2, terminate/2, add_room/2]).

-record(room_manager_state, {rooms}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, []).

add_room(RoomName, RoomPID) ->
  gen_server:cast(?MODULE, {add_room, RoomName, RoomPID}).

get_room_names() ->
  gen_server:call(?MODULE, get_room_names).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
fresh_state() -> #room_manager_state{rooms=orddict:new()}.

handle_add_room(RoomName, RoomPID, State) ->
  RoomDict = State#room_manager_state.rooms,
  NewRoomDict = orddict:put(RoomName, RoomPID, RoomDict),
  NewState = State#room_manager_state{rooms=NewRoomDict},
  {noreply, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
  {ok, fresh_state()}.

handle_call(get_room_names, _From, State) ->
  {reply, State#room_manager_state.rooms}.

handle_cast({add_room, RoomName, RoomPID}, State) ->
  handle_add_room(RoomName, RoomPID, State);
handle_cast(Msg, State) ->
  error_logger:warning_msg("Unexpected cast: ~p~n", [Msg]),
  {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, _, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.

