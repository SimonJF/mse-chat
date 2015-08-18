-module(mse_chat_client).

-behaviour(gen_server).

-record(client_state, {client_id,
                       client_name,
                       client_socket,
                       room_pid
                      }).

% FIXME
-compile(export_all).
-include("mse_chat_records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ClientID, Socket) ->
  gen_server:start_link(?MODULE, [ClientID, Socket], []).

set_name(PID, Name) ->
  gen_server:call(PID, {set_name, Name}).

found_room_pid(ActorPID, RoomName, RoomPID) ->
  % Join room
  gen_server:cast(ActorPID, {room_pid, RoomName, RoomPID}).

room_not_found(ActorPID, RoomName) ->
  % Join room
  gen_server:cast(ActorPID, {room_not_found, RoomName}).

room_create_response(ActorPID, RoomName, IsSuccess) ->
  gen_server:cast(ActorPID, {room_create_response, RoomName, IsSuccess}).

chat_message(ActorPID, SenderName, Message) ->
  gen_server:cast(ActorPID, {chat_message, SenderName, Message}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, State) ->
  Socket = State#client_state.client_socket,
  gen_tcp:send(Socket, Message).

handle_message(Message, State) ->
  StrippedMessage = string:strip(Message),
  SplitMessage = string:tokens(StrippedMessage, ":"),
  [Command|PacketRemainder] = SplitMessage,
  SplitLen = length(SplitMessage),
  NewState =
    if Command == "AUTH" andalso SplitLen > 2 ->
         [Username|[Password|_Rest]] = PacketRemainder,
         handle_auth(Username, Password, State);
       Command == "CHAT" ->
         ChatMessage = string:join(SplitMessage, ":"),
         handle_chat(ChatMessage, State),
         State;
       Command == "CREATE" ->
         [RoomName|_Rest] = PacketRemainder,
         handle_create_room(RoomName, State),
         State;
       Command == "JOIN" ->
         [RoomName|_Rest] = PacketRemainder,
         handle_join_room(RoomName, State);
       Command == "LEAVE" ->
         handle_leave_room(State);
       true ->
         error_logger:warning_msg("Unhandled TCP message: ~p~n", [Command]),
         State
    end,
  {noreply, NewState}.

make_client_data(State) ->
  ClientName = State#client_state.client_name,
  #client_data{client_name=ClientName, client_pid=self()}.


handle_leave_room(State) ->
  RoomPID = State#client_state.room_pid,
  ClientName = State#client_state.client_name,
  mse_chat_room_instance:deregister_client(RoomPID, ClientName),
  State#client_state{room_pid=undefined}.

handle_auth(Username, _Password, State) ->
  % TEMP TEMP TEMP
  send_message("AUTH_OK", State),
  State#client_state{client_name=Username}.

 %if Username == "simon" andalso Password == "password" ->
 %     send_message("AUTH_OK", State),
 %     State#client_state{client_name=Username};
 %   true ->
 %     send_message("AUTH_ERROR", State),
 %     State
 %end.

handle_chat(ChatMessage, State) ->
  ClientName = State#client_state.client_name,
  RoomPID = State#client_state.room_pid,
  mse_chat_room_instance:broadcast_message(RoomPID, ClientName, ChatMessage),
  State.

% Send join room request
handle_join_room(RoomName, State) ->
  mse_chat_room_manager:get_room(RoomName),
  State.

% Receive room found response
handle_room_pid(RoomName, RoomPID, State) ->
  % We need to register, then we're golden.
  ClientName = State#client_state.client_name,
  % TODO: self() isn't going to work with the session type server...
  % Then again we won't be working with PIDs, so eh.
  mse_chat_room_instance:register_client(RoomPID, ClientName, self()),
  send_message("JOINED:" ++ RoomName, State),
  State#client_state{room_pid=RoomPID}.

handle_room_not_found(RoomName, State) ->
  send_message("JOIN_FAIL:" ++ RoomName, State),
  State.

handle_create_room(RoomName, State) ->
  mse_chat_room_manager:create_room(RoomName),
  ok.

handle_create_room_response(RoomName, IsSuccess, State) ->
  if IsSuccess ->
       send_message("CREATED:" ++ RoomName, State);
     not IsSuccess ->
       send_message("CREATE_FAIL:" ++ RoomName, State)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init([ClientID, ClientSocket]) ->
  State = #client_state{client_id=ClientID,
                        client_name=undefined,
                        client_socket=ClientSocket},
  io:format("In MSE chat client init~n"),
  % Start session here.
  inet:setopts(ClientSocket, [{active, true}]),
  {ok, State}.

handle_call({set_name, Name}, _From, State) ->
  NewState = State#client_state{client_name=Name},
  {reply, ok, NewState};
handle_call(Msg, _From, State) ->
  error_logger:error_msg("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

handle_cast({room_create_response, RoomName, IsSuccess}, State) ->
  handle_create_room_response(RoomName, IsSuccess, State),
  {noreply, State};
handle_cast({chat_msg, SenderName, Msg}, State) ->
  send_message("INCOMING_CHAT:" ++ SenderName ++ ":" ++ Msg, State),
  {noreply, State};
handle_cast({room_not_found, RoomName}, State) ->
  handle_room_not_found(RoomName, State),
  {noreply, State};
handle_cast({room_pid, RoomName, RoomPID}, State) ->
  NewState = handle_room_pid(RoomName, RoomPID, State),
  {noreply, NewState};
handle_cast({chat_message, Sender, Message}, State) ->
  Msg = "CHAT_MESSAGE:" ++ Sender ++ ":" ++ Message,
  send_message(Msg, State),
  {noreply, State};
handle_cast(Msg, State) ->
  error_logger:error_msg("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info({tcp, _S, Data}, State) ->
  error_logger:info_msg("Received TCP message: ~p~n", [Data]),
  %gen_tcp:send(S, "AUTH_OK"),
  handle_message(Data, State);
handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

