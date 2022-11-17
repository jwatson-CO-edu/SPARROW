#[
    https://blog.tejasjadhav.xyz/simple-chat-server-in-nim-using-sockets/
]#

########## INIT ####################################################################################

import net #[ All socket related functions, classes, utilities are in the `net` package. 
We could have also used `nativesockets` package which is exclusively for sockets. 
But it handles sockets at very low-level which might just lengthen our code. ]#

import nativesockets # `setBlocking` 

##### Server Init #####

let server: Socket = newSocket() # Initialize our server socket

server.setSockOpt(OptReuseAddr, true) #[ Linger Time: When a socket is closed (especially TCP socket), 
some data which is marked to be sent, still exists in the socket’s send buffer. Unless the buffer is emptied 
or a timeout period (usually 30 seconds) is passed, the system reserves the port. 

To fix this, we use the SO_REUSEADDR property of sockets. If a socket is created with this option enabled, 
instead of throwing an error, it checks whether a socket which is listening on the same port is in lingering state. 
If it is, the socket will use the same address with an assumption that the earlier socket will release the port soon. 
In Nim, we have the `OptReuseAddr` option which can be set as true to enable SO_REUSEADDR property of the socket.]#

server.getFd().setBlocking(false) # In Nim, we enable non-blocking mode using the setBlocking() function of the underlying native socket.

server.bindAddr(Port(5555)) #[ Set the socket to listen to new connections on port 5555 and 
start accepting new connections from other sockets. If address parameter is not mentioned by default, 
the socket will listen to requests from all IP addresses. ]# 



server.listen()

stdout.writeLine("Server: started. Listening to new connections on port 5555...")

#[
  IDEA:
    1. Have an infinte loop (while true) which will keep our server running.
    2. Inside the loop, accept new client connections and add those into our client list.
    3. For each client in the client list, check if there is any data.
    4. If there is any data, print it out.
]#

var clients: seq[Socket] = @[] # Maintain a list of clients

while true: # 1. Have an infinte loop (while true) which will keep our server running.
  try:
    ##### Accept Client #####
    # 2. Inside the loop, accept new client connections and add those into our client list.
    var client: Socket = new(Socket)
    server.accept(client)
    clients.add(client)
    stdout.writeLine("Server: client connected")
  except OSError:
    discard

  var clientsToRemove: seq[int] = @[]

  # 3. For each client in the client list, check if there is any data.
  for index, client in clients:
    
    ##### Process Messages #####
    try:
      let message: string = client.recvLine(timeout = 1) #[ A much better solution here would be to use the `recvLine` method. 
      It handles the complexity of handling the delimited character for us. It uses \r\L as the delimiter. 
      This means, on the client side, for each message we send, we would also need to append this delimiter.  
      
      Fortunately, sockets have an option to run in non-blocking mode. This means, our `accept()` and `recv()` functions will no longer block the code. 
      If they have anything meaningful to return (like an incoming socket connection or a message), 
      they will return that and continue execution. Otherwise, they will throw an error. 
      Which means, we have to keep checking for new connections or messages and also handle the error in case we don’t get any. ]#

      if message == "":  #[ When a socket disconnects, it sends an empty string before disconnection. 
        This can be used as an indication for the server that the client has disconnected. ]#
        clientsToRemove.add(index)

      stdout.writeLine("Server: received from client: ", message)
    except TimeoutError:
        discard

  # Remove disconnected clients
  for index in clientsToRemove:
    clients.del(index)
    stdout.writeLine("Server: client disconnected")

##### Server Shutdown #####

server.close() #[ Close our socket server. It is very important to close socket connections if they are no longer required. 
This is considered a clean way to end TCP connections with proper end-of-connection acknowledgement. ]#