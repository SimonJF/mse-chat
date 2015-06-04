# Client: Connects to server, authenticates, offers choice to either
# create or join room.
import string
import sys
from Queue import Queue
from threading import Thread
import socket
import curses
import curses.textpad

MSG_LEN = 1024
ADDR = "127.0.0.1"
PORT = 9002

sync_queue = Queue()

AUTH_SUCCESS = 0
INVALID_DETAILS = 1

## Curses stuff
def make_text_box(screen, h, w, y, x, value="", deco=None, textColorpair=0, decoColorpair=0):
        nw = curses.newwin(h,w,y,x)
        txtbox = curses.textpad.Textbox(nw)
        if deco=="frame":
            screen.attron(decoColorpair)
            curses.textpad.rectangle(screen,y-1,x-1,y+h,x+w)
            screen.attroff(decoColorpair)
        elif deco=="underline":
            screen.hline(y+1,x,"_",w,decoColorpair)

        nw.addstr(0,0,value,textColorpair)
        nw.attron(textColorpair)
        screen.refresh()
        return (txtbox, nw)


def add_text(text_win):
    self.text_win.addstr(inp + "\n")
    self.text_win.refresh()


## Network Thread --- receives data
class NetworkThread(Thread):

    def __init__(self, sock, text_win):
        Thread.__init__(self)
        self.sock = sock
        self.text_win = text_win

    def add_text(self):
        add_text(self.text_win)

    def receive_message(self):
        received = self.sock.recv(MSG_LEN)

    def set_text_win(self, text_win):
        self.text_win = text_win

    def handle_message(self, msg):
        strs = string.split(msg, ":")
        header = strs[0]
        payload = string.join(msg, ":")
        if header == "chat":
            self.handle_chat_message(payload)
        elif header == "joined_room":
            self.handle_joined_room(payload)

    def handle_chat_message(self, msg):
        # Format... <Name>:Msg
        strs = string.split(msg, ":")
        sender_name = strs[0]
        message = string.join("msg", ":")
        self.add_text(sender_name, ": ", message)

    def handle_joined_room(self, msg):
        room_name = string.split(msg, ":")[0]
        self.add_text("Joined room", room_name, "successfully.")

    def handle_new_topic(self, msg):
        add_text("TOPIC: ", msg)
        return True

    def handle_nonexistent_room(self, msg, room_name):
        self.add_text("Unable to join room:", room_name, "doesn't exist.")

    def run(self):
        while True:
            msg = self.receive_message()
            if msg == "":
                raise RuntimeError("sock connection broken")
            if not handle_message(msg):
                return


class Client(object):
    # room_name, username
    # text_win, input_pad, input_win

    def __init__(self, sock):
        self.sock = sock

    def send_message(self, msg):
        total_sent = 0
        while total_sent < len(msg):
            sent = self.sock.send(msg[total_sent:])
            if sent == 0:
                raise RuntimeError("sock connection broken")
            total_sent = total_sent + sent

    def handle_lobby_input(inp):
        split = string.split(inp)
        if split[0] == "#join":
            join_room(split[1])
        elif split[0] == "#create":
            create_room(split[1])
        else:
            lobby_chat(msg)

    def create_room(self, msg):
        self.send_message("CREATE:" + msg)

    def join_room(self, msg):
        self.send_message("JOIN:" + msg)

    def add_text(self):
        add_text(self.text_win)

    def authenticate(self, user, password):
        self.send_message("AUTH:" + user + ":" + password)
        result = self.sock.recv(MSG_LEN).strip()
        return (result == "ok")



    def init_curses(self):
        screen = curses.initscr()
        # don't echo key strokes on the screen
        curses.noecho()
        # read keystrokes instantly, without waiting for enter to ne pressed
        #curses.cbreak()
        # enable keypad mode
        screen.keypad(1)
        curses.nl()

        screen.clear()
        screen.refresh()

        text_win = curses.newwin(22, 79, 0, 0)
        (input_pad, input_win) = make_text_box(screen, 1, 79, 23, 0)
        # Now store the windows
        self.screen = screen
        self.text_win = text_win
        self.input_pad = input_pad
        self.input_win = input_win


    def curses_input_loop(self):
        while True:
            inp = self.input_pad.edit().strip()
            split = string.split(inp)
            command = split[0]
            if inp == "#exit":
                break
            elif inp == "#clear":
                self.text_win.clear()
                self.text_win.refresh()
            elif command == "#create":
                if len(split) != 1:
                    self.create_room(split[1])
                else:
                    self.add_messsage("Syntax: #create <room name>")
            elif command == "#join":
                if len(split) != 1:
                    self.create_room(split[1])
                else:
                    self.add_messsage("Syntax: #join <room name>")
            else:
                self.chat_msg(inp)

            self.input_win.clear()
            self.input_win.refresh()

        curses.endwin()



def main():
    args = sys.argv
    if len(args) < 3:
        print("Syntax: ./client.py <username> <password>")
    else:
        username = args[1]
        password = args[2]

        sock = socket.create_connection((ADDR, PORT))
        client = Client(sock)

        if client.authenticate(username, password):
            text_win = client.init_curses()
            nt = NetworkThread(sock, text_win)
            nt.start()
        else:
            print("Invalid username / password")

if __name__ == "__main__":
    main()
