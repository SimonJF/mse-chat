-module(mse_chat_config).
-compile(export_all).

config() ->
  [{mse_chat_client, [{"ChatServer", ["ClientThread"]},
                      {"ChatSession", ["ClientThread"]}]},
   {mse_chat_room_manager, [{"ChatServer", ["RoomRegistry"]}]},
   {mse_chat_room_instance, [{"ChatSession", ["ChatRoom"]}]}].

