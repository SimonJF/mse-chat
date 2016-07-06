-module(mse_chat_room_manager).
-behaviour(ssa_gen_server).

-compile(export_all).

-record(room_manager_state, {rooms}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  ssa_gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room_names(ConvKey) ->
  conversation:call(ConvKey, "RoomRegistry", "listRooms", [], []).

get_room(ConvKey, RoomName) ->
  conversation:send(ConvKey, ["RoomRegistry"], "lookupRoom", [], [RoomName]).

create_room(ConvKey, RoomName) ->
  conversation:send(ConvKey, ["RoomRegistry"], "createRoom", [], [RoomName]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
fresh_state() -> #room_manager_state{rooms=orddict:new()}.

add_room(RoomName, RoomPID, State) ->
  RoomDict = State#room_manager_state.rooms,
  NewRoomDict = orddict:store(RoomName, RoomPID, RoomDict),
  State#room_manager_state{rooms=NewRoomDict}.

handle_get_room(ConvKey, RoomName, State) ->
  RoomDict = State#room_manager_state.rooms,
  case orddict:find(RoomName, RoomDict) of
    {ok, RoomPID} ->
      mse_chat_client:found_room_pid(ConvKey, RoomName, RoomPID);
    error ->
      mse_chat_client:room_not_found(ConvKey, RoomName)
  end.

handle_create_room(ConvKey, RoomName, State) ->
  io:format("In handle create room~n"),
  RoomDict = State#room_manager_state.rooms,
  RoomExists = orddict:is_key(RoomName, RoomDict),
  if RoomExists ->
       mse_chat_client:room_exists(ConvKey, RoomName),
       State;
     not RoomExists ->
       {ok, Pid} = mse_chat_room_instance_sup:create_new_room(RoomName),
       mse_chat_client:room_create_success(ConvKey, RoomName),
       add_room(RoomName, Pid, State)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
ssactor_init(_Args, _Monitor) ->
  error_logger:info_msg("Chat room manager started.~n", []),
  fresh_state().

ssactor_join(_, _, _, State) ->
  error_logger:info_msg("Chat room manager accepted invitation~n"),
  {accept, State}.

ssactor_conversation_established("ChatServer", "RoomRegistry", _CID, ConvKey, State) ->
  error_logger:info_msg("Conv established (room manager)~n"),
  {ok, State}.

ssactor_handle_message("ChatServer", "RoomRegistry", _, _, "createRoom",
                       [RoomName], State, ConvKey) ->
  NewState = handle_create_room(ConvKey, RoomName, State),
  {ok, NewState};
ssactor_handle_message("ChatServer", "RoomRegistry", _, _, "lookupRoom",
                       [RoomName], State, ConvKey) ->
  handle_get_room(ConvKey, RoomName, State),
  {ok, State}.

ssactor_subsession_complete(_, _, State, _) -> {ok, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.
ssactor_become(_, _, _, _, _, State) -> {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  {ok, State}.

handle_cast(Msg, State) ->
  error_logger:warning_msg("Unexpected cast: ~p~n", [Msg]),
  {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, _, State) -> {ok, State}.
terminate(Reason, _State) ->
  error_logger:error_msg("MSE Chat room manager terminating because: ~p~n", [Reason]).

