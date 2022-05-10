#[
    https://blog.tejasjadhav.xyz/simple-chat-server-in-nim-using-sockets/
]#

########## INIT ####################################################################################

import net #[ All socket related functions, classes, utilities are in the `net` package. 
We could have also used `nativesockets` package which is exclusively for sockets. 
But it handles sockets at very low-level which might just lengthen our code. ]#

##### Server Init #####

let server: Socket = newSocket() # Initialize our server socket

#[ Set the socket to listen to new connections on port 5555 and start accepting new connections from other sockets. 
If address parameter is not mentioned by default, the socket will listen to requests from all IP addresses. ]# 
server.bindAddr(Port(5555)) 
server.listen()

stdout.writeLine("Server: started. Listening to new connections on port 5555...")

##### Accept Client #####

var client: Socket = new(Socket)
server.accept(client)
stdout.writeLine("Server: client connected")

##### Process Messages #####

while true: # Server will keep listening to new messages from all connected clients.
  
  let message: string = client.recv(10) # Read a few characters from the socket.

  #[ When a socket disconnects, it sends an empty string before disconnection. 
  This can be used as an indication for the server that the client has disconnected. 
  Since we are dealing with only one client, we’ll stop receiving additional messages and exit the loop. ]#
  if message == "":
    break

  stdout.writeLine("Server: received from client: ", message)


##### Server Shutdown #####

server.close() #[ Close our socket server. It is very important to close socket connections if they are no longer required. 
This is considered a clean way to end TCP connections with proper end-of-connection acknowledgement. ]#