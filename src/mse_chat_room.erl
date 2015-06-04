-module(mse_chat_room).
-behaviour(gen_server).
-define(MAXT, 3600).
-define(MAX, 4).
-include("mse_chat_records.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, code_change/3,
         handle_info/2, terminate/2, add_client/1]).
-record(chat_room_state, {room_name,
                          room_topic,
                          room_members}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_client(ClientData) ->
  gen_server:cast(?MODULE, {register_participant, ClientData}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
fresh_state(RoomName) ->
  #chat_room_state{room_name=RoomName,
                   room_topic="",
                   room_members=orddict:new()}.

handle_add_client(ClientData, State) ->
  ClientName = ClientData#client_data.client_name,
  ClientData = ClientData#client_data.client_pid,
  MemberList = State#chat_room_state.room_members,
  NewMemberList = orddict:put(ClientName, ClientData, MemberList),
  NewState = State#chat_room_state{room_members=NewMemberList},
  broadcast_client_joined(ClientName, NewState),
  {noreply, NewState}.

handle_set_topic(Topic, State) ->
  NewState = State#chat_room_state{room_topic=Topic},
  broadcast_new_topic(Topic, State),
  {noreply, NewState}.

broadcast_client_joined(ClientName, State) ->
  ClientList = orddict:to_list(State#chat_room_state.room_members),
  lists:foreach(fun(_, Pid) ->
                    mse_chat_client:send_new_client_joined(Pid, ClientName) end,
                ClientList),
  ok.

broadcast_new_topic(Topic, State) ->
  ClientList = orddict:to_list(State#chat_room_state.room_members),
  RoomName = State#chat_room_state.room_topic,
  lists:foreach(fun(ClientName, Pid) ->
                    mse_chat_client:send_new_topic(Topic, ClientName) end,
                ClientList),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([RoomName]) -> {ok, fresh_state(RoomName)}.

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({register_participant, ClientData}, State) ->
  handle_add_client(ClientData, State);
handle_cast({set_topic, NewTopic}, State) ->
  handle_set_topic(NewTopic, State);
handle_cast(Msg, State) ->
  error_logger:warning_msg("Unexpected cast: ~p~n", [Msg]),
  {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, _, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
