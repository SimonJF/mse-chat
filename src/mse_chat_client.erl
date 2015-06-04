-module(mse_chat_client).

-behaviour(gen_server).

-record(client_state, {client_id,
                       client_name,
                       client_socket}).

% FIXME
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ClientID, Socket) ->
  gen_server:start_link(?MODULE, [ClientID, Socket], []).

set_name(PID, Name) ->
  gen_server:call(PID, {set_name, Name}).

% TODO: send_new_topic


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%
handle_message(Message, State) ->
  % TODO
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
  gen_tcp:send(S, "ok"),
  {noreply, State};
handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

