-module(mse_chat_room_instance).

-compile(export_all).

-record(room_state, {room_name,
                     room_members}).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Args) ->
  gen_server:start_link(?MODULE, Args).

register_client(ClientName, ClientPID) ->
  gen_server:call(?MODULE, {register_client, ClientName, ClientPID}).

deregister_client(ClientName) ->
  gen_server:call(?MODULE, {deregister_client, ClientName}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%
handle_register_client(Name, PID, State) ->
  RoomMembers = State#room_state.room_members,
  NewRoomMembers = orddict:store(Name, PID, RoomMembers),
  State#room_state{room_members=NewRoomMembers}.

handle_deregister_client(Name, State) ->
  RoomMembers = State#room_state.room_members,
  NewRoomMembers = orddict:erase(Name, RoomMembers),
  State#room_state{room_members=NewRoomMembers}.

handle_broadcast_message(SenderName, Message, State) ->
  RoomMembers = orddict:to_list(State#room_state.room_members),
  lists:foreach(fun({Name, Endpoint}) ->
                    if Name =/= SenderName ->
                      mse_chat_client:message(Endpoint, {chat_message, SenderName, Message});
                      Name == SenderName -> ok
                    end
                    end,
               RoomMembers).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
  State = #room_state{room_members=orddict:new()},
  {ok, State}.

handle_call({register_client, Name, PID}, _From, State) ->
  NewState = handle_register_client(Name, PID, State),
  {reply, ok, NewState};
handle_call({deregister_client, Name}, _From, State) ->
  NewState = handle_deregister_client(Name, State),
  {reply, ok, NewState};
handle_call(Msg, _From, State) ->
  error_logger:error_message("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

handle_cast({chat_message, ClientName, Message}, State) ->
  handle_broadcast_message(ClientName, Message, State),
  {noreply, State};
handle_cast(Msg, State) ->
  error_logger:error_message("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info(Msg, State) ->
  error_logger:error_message("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.
