-module(mse_chat_client).

-behaviour(ssa_gen_server).

-record(client_state, {client_id,
                       client_name,
                       client_socket,
                       monitor_pid
                      }).

% FIXME
-compile(export_all).
-include("mse_chat_records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ClientID, Socket) ->
  ssa_gen_server:start_link(?MODULE, [ClientID, Socket], []).

found_room_pid(ConvKey, RoomName, RoomPID) ->
  conversation:send(ConvKey, ["ClientThread"], "roomPID", [], [RoomName, RoomPID]).

room_not_found(ConvKey, RoomName) ->
  conversation:send(ConvKey, ["ClientThread"], "roomNotFound", [], [RoomName]).

room_exists(ConvKey, RoomName) ->
  conversation:send(ConvKey, ["ClientThread"], "roomExists", [], [RoomName]).

room_create_success(ConvKey, RoomName) ->
  conversation:send(ConvKey, ["ClientThread"], "createRoomSuccess", [], [RoomName]).

chat_message(ConvKey, SenderName, Message) ->
  conversation:send(ConvKey, ["ClientThread"], "incomingChatMessage",
                    [], [SenderName, Message]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, State) ->
  Socket = State#client_state.client_socket,
  gen_tcp:send(Socket, Message).

handle_message(Message, State) ->
  MonitorPID = State#client_state.monitor_pid,
  StrippedMessage = string:strip(Message),
  SplitMessage = string:tokens(StrippedMessage, ":"),
  [Command|PacketRemainder] = SplitMessage,
  SplitLen = length(SplitMessage),
  NewState =
    if Command == "AUTH" andalso SplitLen > 2 ->
         [Username|[Password|_Rest]] = PacketRemainder,
         handle_auth(Username, Password, State);
       Command == "CHAT" ->
         [_|SplitChatMessage] = SplitMessage,
         ChatMessage = string:join(SplitChatMessage, ":"),
         conversation:become(MonitorPID, chat_session, "ClientThread",
                             chat, [ChatMessage]),
         State;
       Command == "CREATE" ->
         [RoomName|_Rest] = PacketRemainder,
         conversation:become(MonitorPID, main_thread, "ClientThread",
                             create_room, [RoomName]),
         State;
       Command == "JOIN" ->
         [RoomName|_Rest] = PacketRemainder,
         conversation:become(MonitorPID, main_thread, "ClientThread",
                             join_room, [RoomName]),
         State;
       Command == "LEAVE" ->
         conversation:become(MonitorPID, chat_session, "ClientThread",
                             leave_room, []),
         State;
       true ->
         error_logger:warning_msg("Unhandled TCP message: ~p~n", [Command]),
         State
    end,
  {noreply, NewState}.

make_client_data(State) ->
  ClientName = State#client_state.client_name,
  #client_data{client_name=ClientName, client_pid=self()}.


handle_leave_room(ConvKey, State) ->
  ClientName = State#client_state.client_name,
  conversation:subsession_complete(ConvKey, ok),
  State.

handle_auth(Username, _Password, State) ->
  % TEMP TEMP TEMP
  send_message("AUTH_OK", State),
  State#client_state{client_name=Username}.

handle_chat(ConvKey, ChatMessage, State) ->
  ClientName = State#client_state.client_name,
  mse_chat_room_instance:broadcast_message(ConvKey, ClientName, ChatMessage),
  State.

% Send join room request
handle_join_room(ConvKey, RoomName) ->
  mse_chat_room_manager:get_room(ConvKey, RoomName).

handle_create_room(ConvKey, RoomName) ->
  mse_chat_room_manager:create_room(ConvKey, RoomName),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

ssactor_join(_, _, _, State) ->
  error_logger:info_msg("Accepted invitation~n"),
  {accept, State}.

ssactor_conversation_established("ChatServer", "ClientThread", _CID, ConvKey, State) ->
  error_logger:info_msg("Conv established~n"),
  conversation:register_conversation(main_thread, ConvKey),
  {ok, State};
ssactor_conversation_established("ChatSession", "ClientThread", _CID, ConvKey, State) ->
  conversation:register_conversation(chat_session, ConvKey),
  {ok, State}.


ssactor_conversation_error(_PN, _RN, Error, State) ->
  error_logger:info_msg("Error setting up session: ~p~n", [Error]),
  {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  {ok, State}.


ssactor_init([ClientID, ClientSocket], MonitorPID) ->
  State = #client_state{client_id=ClientID,
                        client_name=undefined,
                        client_socket=ClientSocket,
                        monitor_pid=MonitorPID
                       },
  io:format("In MSE chat client init~n"),
  % Start session here.
  conversation:start_conversation(MonitorPID, "ChatServer", "ClientThread"),
  inet:setopts(ClientSocket, [{active, true}]),
  State.

ssactor_handle_call(_, _, _, _, Msg, _, State, _) ->
  error_logger:error_msg("Unhandled ssactor call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

%%% Out-of-room actions
ssactor_handle_message("ChatServer", "ClientThread", _, _, "createRoomSuccess",
                       [RoomName], State, _ConvKey) ->
  send_message("CREATED:" ++ RoomName, State),
  {ok, State};
ssactor_handle_message("ChatServer", "ClientThread", _, _, "roomExists", [RoomName],
                       State, _ConvKey) ->
  send_message("CREATE_FAIL:" ++ RoomName, State),
  {ok, State};
ssactor_handle_message("ChatServer", "ClientThread", _, _, "roomNotFound", [RoomName],
                       State, _ConvKey) ->
  send_message("JOIN_FAIL:" ++ RoomName, State),
  {ok, State};
ssactor_handle_message("ChatServer", "ClientThread", _, _, "roomPID", [RoomName, RoomPID],
                       State, ConvKey) ->
  conversation:start_subsession(ConvKey, "ChatSession", ["ClientThread"],
                                [{"ChatRoom", RoomPID}]),
  send_message("JOINED:" ++ RoomName, State),
  {ok, State};
%%% In-room actions
ssactor_handle_message("ChatSession", "ClientThread", _, _, "incomingChatMessage",
                       [Username, Message], State, _ConvKey) ->
  send_message("CHAT_MESSAGE" ++ ":" ++ Username ++ ":" ++ Message, State),
  {ok, State}.


ssactor_become("ChatServer", "ClientThread", create_room, [RoomName],
               ConvKey, State) ->
  handle_create_room(ConvKey, RoomName),
  {ok, State};
ssactor_become("ChatServer", "ClientThread", join_room, [RoomName],
               ConvKey, State) ->
  handle_join_room(ConvKey, RoomName),
  {ok, State};
ssactor_become("ChatSession", "ClientThread", leave_room, [],
               ConvKey, State) ->
  NewState = handle_leave_room(ConvKey, State),
  {ok, NewState};
ssactor_become("ChatSession", "ClientThread", chat, [Message],
               ConvKey, State) ->
  handle_chat(ConvKey, Message, State),
  {ok, State}.


ssactor_subsession_complete(_, _, State, _) -> {ok, State}.

ssactor_subsession_failed(_, "ParticipantOffline", State, _) ->
  error_logger:info_msg("Chat room died.~n"),
  % Here, it'd be good to tell the client that this happened.
  {ok, State};
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.

ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(Msg, _From, State) ->
  error_logger:error_msg("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  error_logger:error_msg("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info({tcp, _S, Data}, State) ->
  %error_logger:info_msg("Received TCP message: ~p~n", [Data]),
  %gen_tcp:send(S, "AUTH_OK"),
  handle_message(Data, State);
handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

