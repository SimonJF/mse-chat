# Client: Connects to server, authenticates, offers choice to either
# create or join room.
MSG_LEN = 1024
ADDR = "127.0.0.1"
PORT = 9002

class NetworkThread(Thread):

    def __init__(self, socket):
        Thread.__init__()
        self.socket = socket

    def receive_message():
        received = self.socket.recv(MSG_LEN)

    def handle_message(msg):
        strs = string.split(msg, ":")
        header = strs[0]
        payload = string.join(msg, ":")
        if header == "chat":
            handle_chat_message(msg)


    def handle_chat_message(msg):
        # Format... <Name>:Msg
        strs = string.split(msg, ":")
        sender_name = strs[0]
        message = string.join("msg", ":")
        print(sender_name, ": ", message)
        return True

    def handle_joined_room(msg, room_name):
        print("Joined room", room_name, "successfully.")

    def handle_new_topic(msg):
        print("TOPIC: ", msg)
        return True

    def handle_nonexistent_room(msg, room_name):
        print("Unable to join room:", room_name, "doesn't exist.")

    def run(self):
        while True:
            msg = self.receive_message()
            if msg == "":
                raise RuntimeError("Socket connection broken")
            if not handle_message(msg):
                return


class Client(object):
    # room_name, user

    def send_message(self, msg):
        total_send = 0
        while total_sent < len(msg):
            sent = self.socket.send(msg[total_sent:])
            if sent == 0:
                raise RuntimeError("Socket connection broken")
            total_sent = total_sent + sent

    def lobby_loop():
        while True:
            input = raw_get(self.name + " @ Lobby >")


    def room_loop():
        pass

    def join_room(self, msg):
        self.send_message("JOIN:" + msg)

    def authenticate(user, password, socket):
        pass

def main():
   sock = socket.create_connection((ADDR, PORT))
   nt = NetworkThread(sock)
   nt.start()
   lobby_loop()

if __name__ == "__main__":
    main()
