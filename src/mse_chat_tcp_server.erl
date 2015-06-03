-module(mse_chat_tcp_server).
-export([init/1]).

%%%
%%% Chat TCP Server: Listens for connections, accepts, adds to
%%% registry and starts new client thread
%%%


init([Port]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port),
  accept_loop(ListenSocket, 0).


accept_loop(ListenSocket, ConnectionCount) ->
  AcceptRes = gen_tcp:accept(ListenSocket),
  case AcceptRes of
    {ok, ClientSocket} ->
      {ok, _} = mse_chat_client_sup:new_client(ConnectionCount, ClientSocket),
      accept_loop(ListenSocket, ConnectionCount + 1);
    {error, Err} ->
      error_logger:error_msg("Error accepting connection: ~p~n", [Err]),
      exit(error_accepting)
  end.
