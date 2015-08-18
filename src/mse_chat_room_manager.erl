-module(mse_chat_room_manager).
-behaviour(gen_server).

-compile(export_all).

-record(room_manager_state, {rooms}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room_names() ->
  gen_server:call(?MODULE, get_room_names).

get_room(RoomName) ->
  gen_server:cast(?MODULE, {get_room, RoomName, self()}).

create_room(RoomName) ->
  gen_server:cast(?MODULE, {create_room, RoomName, self()}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
fresh_state() -> #room_manager_state{rooms=orddict:new()}.

add_room(RoomName, RoomPID, State) ->
  RoomDict = State#room_manager_state.rooms,
  NewRoomDict = orddict:store(RoomName, RoomPID, RoomDict),
  State#room_manager_state{rooms=NewRoomDict}.

handle_get_room(RoomName, ActorPID, State) ->
  RoomDict = State#room_manager_state.rooms,
  case orddict:find(RoomName, RoomDict) of
    {ok, RoomPID} ->
      mse_chat_client:found_room_pid(ActorPID, RoomName, RoomPID);
    error ->
      mse_chat_client:room_not_found(ActorPID, RoomName)
  end.

handle_create_room(RoomName, ActorPID, State) ->
  io:format("In handle create room~n"),
  RoomDict = State#room_manager_state.rooms,
  RoomExists = orddict:is_key(RoomName, RoomDict),
  if RoomExists ->
       mse_chat_client:room_create_response(ActorPID, RoomName, false),
       {noreply, State};
     not RoomExists ->
       {ok, Pid} = mse_chat_room_instance_sup:create_new_room([RoomName]),
       mse_chat_client:room_create_response(ActorPID, RoomName, true),
       {noreply, add_room(RoomName, Pid, State)}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
  {ok, fresh_state()}.

handle_call(get_room_names, _From, State) ->
  {reply, State#room_manager_state.rooms}.

handle_cast({create_room, RoomName, ActorPID}, State) ->
  handle_create_room(RoomName, ActorPID, State);
handle_cast({get_room, RoomName, ActorPID}, State) ->
  handle_get_room(RoomName, ActorPID, State),
  {noreply, State};
handle_cast(Msg, State) ->
  error_logger:warning_msg("Unexpected cast: ~p~n", [Msg]),
  {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, _, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.

