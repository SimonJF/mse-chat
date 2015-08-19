-module(mse_chat_main).
-export([main/0]).

main() ->
  {ok, _Pid} = conversation:initialise("scribble_specs", mse_chat_config:config()),
  {ok, Pid} = mse_chat_sup:start_link().

