-module(mse_client_registry).
-behaviour(gen_server).

-record(registry_state, {client_dict}).
% FIXME
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_client(ClientName, ClientPID) ->
  gen_server:call(?MODULE, {register_client, ClientName, ClientPID}).

deregister_client(ClientName) ->
  gen_server:call(?MODULE, {deregister_client, ClientName}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal         %%%
%%%%%%%%%%%%%%%%%%%%%%%%
handle_register_client(Name, PID, State) ->
  ClientDict = State#registry_state.client_dict,
  NewClientDict = orddict:store(Name, PID, ClientDict),
  State#registry_state{client_dict=NewClientDict}.

handle_deregister_client(Name, State) ->
  ClientDict = State#registry_state.client_dict,
  NewClientDict = orddict:erase(Name, ClientDict),
  State#registry_state{client_dict=NewClientDict}.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
  State = #registry_state{client_dict=orddict:new()},
  {ok, State}.

handle_call({register_client, Name, PID}, _From, State) ->
  NewState = handle_register_client(Name, PID, State),
  {reply, ok, NewState};
handle_call({deregister_client, Name}, _From, State) ->
  NewState = handle_deregister_client(Name, State),
  {reply, ok, NewState};
handle_call(Msg, _From, State) ->
  error_logger:error_msg("Unhandled call in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  error_logger:error_msg("Unhandled cast in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.


handle_info(Msg, State) ->
  error_logger:error_msg("Unhandled info in client ~p: ~p~n",
                             [self(), Msg]),
  {noreply, State}.

terminate(Reason, _) ->
  error_logger:error_msg("Client registry terminating for reason: ~p~n", [Reason]).

code_change(_OldVsn, State, _) -> {ok, State}.

