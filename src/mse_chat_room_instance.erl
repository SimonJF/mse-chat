-module(mse_chat_room_instance).
-behaviour(ssa_gen_server).
-compile(export_all).

-record(room_state, {room_name,
                     room_members,
                     monitor_pid
                    }).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(RoomName) ->
  io:format("In chat room instance start~n"),
  ssa_gen_server:start_link(?MODULE, [RoomName], []).

broadcast_message(ConvKey, ClientName, Message) ->
  conversation:send(ConvKey, ["ChatRoom"], "outgoingChatMessage", [],
                    [ClientName, Message]).

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

handle_broadcast_message(ConvKey, SenderName, Message, State) ->
  error_logger:info_msg("Broadcasting chat message ~p from ~p~n", [Message, SenderName]),
  RoomMembers = orddict:to_list(State#room_state.room_members),
  lists:foreach(fun(CID) ->
                      conversation:become(ConvKey, CID, "ChatRoom",
                                          broadcast, [SenderName, Message])
                    end,
               RoomMembers).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

ssactor_join(_, _, _, State) ->
  error_logger:info_msg("Chat room accepted invitation~n"),
  {accept, State}.

ssactor_conversation_established("ChatSession", "ChatRoom", CID, ConvKey, State) ->
  conversation:register_conversation(CID, ConvKey),
  ClientList = State#room_state.room_members,
  NewClientList = [CID|ClientList],
  NewState = State#room_state{room_members=NewClientList},
  {ok, NewState}.

ssactor_init([RoomName], _Monitor) ->
  #room_state{room_name=RoomName, room_members=orddict:new()}.

ssactor_handle_message("ChatSession", "ChatRoom", _, _, "outgoingChatMessage",
                       [SenderName, ChatMessage], State, ConvKey) ->
  handle_broadcast_message(ConvKey, SenderName, ChatMessage, State),
  {ok, State}.
%mse_chat_client:chat_message(Endpoint, SenderName, MessageContents)

ssactor_become("ChatSession", "ChatRoom", broadcast, [SenderName, Message], ConvKey, State) ->
  mse_chat_client:chat_message(ConvKey, SenderName, Message),
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  ClientList = State#room_state.room_members,
  NewClientList = lists:filter(fun(ListCID) -> CID =/= ListCID end, ClientList),
  {ok, State#room_state{room_members=NewClientList}}.

ssactor_subsession_complete(_, CID, State, _) ->
  ClientList = State#room_state.room_members,
  NewClientList = lists:filter(fun(ListCID) -> CID =/= ListCID end, ClientList),
  {ok, State#room_state{room_members=NewClientList}}.

ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

% handle_call({register_client, Name, PID}, _From, State) ->
%   NewState = handle_register_client(Name, PID, State),
%   {reply, ok, NewState};
% handle_call({deregister_client, Name}, _From, State) ->
%   NewState = handle_deregister_client(Name, State),
%   {reply, ok, NewState};

handle_call(Msg, _From, State) ->
  error_logger:error_msg("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

%handle_cast({chat_message, ClientName, Message}, State) ->
%  handle_broadcast_message(ClientName, Message, State),
%  {noreply, State};
handle_cast(Msg, State) ->
  error_logger:error_msg("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

