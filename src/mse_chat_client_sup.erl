-module(mse_chat_client_sup).
-behaviour(supervisor).
-define(MAXT, 4).
-define(MAXR, 4).

-export([start_link/0, new_client/2, init/1]).

%%% Chat client supervisor: Supervisor of all chat clients

%%%%%%%%%%%%%%%%%%%%%%%%
%%% API              %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Start and register the supervisor
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%
%%%start_child(SupRef, ChildSpec) -> startchild_ret()

%%% Start and register a new client
new_client(ClientID, ClientSocket) ->
  supervisor:start_child(?MODULE, [ClientID, ClientSocket]).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks        %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
  ChildType = {
    mse_chat_client,
    {mse_chat_client, start_link, []},
    temporary, 2000, worker, [mse_chat_client]
  },

  RestartStrategyTuple = {simple_one_for_one, ?MAXT, ?MAXR},
  {ok, {RestartStrategyTuple, [ChildType]}}.

