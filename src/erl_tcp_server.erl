-module(erl_tcp_server).
-compile(export_all).

main() ->
  {ok, ListenSocket} = gen_tcp:listen(9001, [{active, false}]),
  accept_loop(ListenSocket, 0).

accept_loop(ListenSocket, ConnectionCount) ->
  AcceptRes = gen_tcp:accept(ListenSocket),
  case AcceptRes of
    {ok, Socket} -> spawn(?MODULE, recv_loop, [Socket, ConnectionCount]),
                    accept_loop(ListenSocket, ConnectionCount + 1);
    {error, Err} ->
      error_logger:error_msg("Error accepting connection: ~p~n",
                            [Err])
  end.

recv_loop(Socket, ID) ->
  io:format("Socket: ~p~n", [Socket]),
  RecvRes = gen_tcp:recv(Socket, 0),
  case RecvRes of
    {ok, Data} ->
      error_logger:info_msg("Received on process ~p: ~p~n", [ID, Data]),
      recv_loop(Socket, ID);
    {error, Err} ->
      error_logger:error_msg("Error receiving on process ~p: ~p~n",
                             [ID, Err]),
      gen_tcp:close(Socket)
  end.

