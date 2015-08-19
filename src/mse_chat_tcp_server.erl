-module(mse_chat_tcp_server).
-export([init/1,start_link/1]).

%%%
%%% Chat TCP Server: Listens for connections, accepts, adds to
%%% registry and starts new client thread
%%%


start_link(Port) ->
  Pid = spawn(?MODULE, init, [Port]),
  link(Pid),
  {ok, Pid}.


init(Port) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}, {ip, {127, 0, 0, 1}}]),
  error_logger:info_msg("Starting TCP Server on Port ~p: ~p~n", [Port, ListenSocket]),
  accept_loop(ListenSocket, 0).


accept_loop(ListenSocket, ConnectionCount) ->
  AcceptRes = gen_tcp:accept(ListenSocket),
  case AcceptRes of
    {ok, ClientSocket} ->
      {ok, _} =
        {ok, Pid} = mse_chat_client_sup:new_client(ConnectionCount, ClientSocket),
        gen_tcp:controlling_process(ClientSocket, Pid),
        accept_loop(ListenSocket, ConnectionCount + 1);
    {error, Err} ->
      error_logger:error_msg("Error accepting connection: ~p~n", [Err]),
      exit(error_accepting)
  end.
