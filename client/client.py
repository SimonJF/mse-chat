# Client: Connects to server, authenticates, offers choice to either
# create or join room.
MSG_LEN = 1024


class NetworkThread(Thread):

    def __init__(self, socket):
        Thread.__init__()
        self.socket = socket

    def receive_message():
        pass

    def handle_message(msg):
        strs = string.split(msg, ":")
        header = strs[0]
        payload = string.join(msg, ":")

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



def send_message(socket, msg):
    total_send = 0
    while total_sent < len(msg):
        ssent = self.sock.send(msg[total_sent:])
        if sent == 0:
            raise RuntimeError("Socket connection broken")
        total_sent = total_sent + sent

def lobby_loop():
    pass

def room_loop():
    pass

def join_room():
    pass

def authenticate(user, password, socket):
    pass

def connect_to_server():
    pass

def main():
    pass

if __name__ == "__main__":
    main()
