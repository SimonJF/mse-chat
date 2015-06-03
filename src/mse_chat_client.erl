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
start_link(Args) ->
  gen_server:start_link(?MODULE, Args).

set_name(PID, Name) ->
  gen_server:call(PID, {set_name, Name}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init([ClientID, ClientSocket]) ->
  State = #client_state{client_id=ClientID,
                        client_name=undefined,
                        client_socket=ClientSocket},
  {ok, State}.

handle_call({set_name, Name}, _From, State) ->
  NewState = State#client_state{client_name=Name},
  {reply, ok, NewState};
handle_call(Msg, _From, State) ->
  error_logger:error_message("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  error_logger:error_message("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info({tcp, _S, Data}, State) ->
  error_logger:info_message("Received TCP message: ~p~n", [Data]),
  {noreply, State};
handle_info(Msg, State) ->
  error_logger:error_message("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

code_change(_OldVsn, State, _) -> {ok, State}.

