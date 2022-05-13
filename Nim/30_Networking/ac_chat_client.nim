#[
    https://blog.tejasjadhav.xyz/simple-chat-server-in-nim-using-sockets/
    nim compile --run -o:./exec/ac_chat_client -r ac_chat_client.nim
]#

import net

#[ Similar to the server, we need a socket object. The only difference here is, unlike server, which listens to connections, 
a client knows the server address and connects to it using the connect method. Thus, a client needs to have the address and port of the server. ]#
let client: Socket = newSocket()
client.connect("127.0.0.1", Port(5555))

stdout.writeLine("Client: connected to server on address 127.0.0.1:5555")

#[ Prompt the user for a chat message and using the client socket, send it to the server. 
Also, we are using `readLine` so that the messsage gets stored only when user presses Enter ]#
while true:
  stdout.write("> ")
  let message: string = stdin.readLine()
  # client.send(message)
  
  client.send(message & "\r\L") #[ The server uses \r\L as the delimiter. 
  This means, on the client side, for each message we send, we would also need to append this delimiter.  ]#

client.close()