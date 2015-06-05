-module(mse_chat_client).

-behaviour(gen_server).

-record(client_state, {client_id,
                       client_name,
                       client_socket,
                       room_pid
                      }).

% FIXME
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ClientID, Socket) ->
  gen_server:start_link(?MODULE, [ClientID, Socket], []).

set_name(PID, Name) ->
  gen_server:call(PID, {set_name, Name}).

found_room_pid(ActorPID, RoomPID) ->
  % Join room
  gen_server:cast(ActorPID, {room_pid, RoomPID}).

  % TODO: send_new_topic


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, State) ->
  Socket = State#client_state.client_socket,
  gen_tcp:send(Socket, Message).

handle_message(Message, State) ->
  StrippedMessage = string:strip(Message),
  SplitMessage = string:tokens(StrippedMessage, " "),
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
       Command ->
         error_logger:warning_msg("Unhandled TCP message: ~p~n", [Command]),
         State
    end,
  {noreply, NewState}.


handle_auth(Username, Password, State) ->
  % TEMP TEMP TEMP
  if Username == "simon" andalso Password == "password" ->
       send_message("AUTH_OK", State),
       State#client_state{client_name=Username};
     true ->
       send_message("AUTH_ERROR", State),
       State
  end.

handle_chat(ChatMessage, State) ->
  ClientName = State#client_state.client_name,
  RoomPID = State#client_state.room_pid,
  mse_chat_room_instance:broadcast_message(RoomPID, ClientName, ChatMessage),
  State.

handle_join_room(RoomName, State) ->
  ok.

handle_room_pid(RoomName, RoomPID, State) ->
  ok.

handle_room_not_found(RoomName, RoomPID, State) ->
  ok.



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

handle_cast(Msg, State) ->
  error_logger:error_msg("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info({tcp, S, Data}, State) ->
  error_logger:info_msg("Received TCP message: ~p~n", [Data]),
  gen_tcp:send(S, "AUTH_OK"),
  {noreply, State};
handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

