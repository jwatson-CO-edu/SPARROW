#[
    https://blog.tejasjadhav.xyz/simple-chat-server-in-nim-using-sockets/
    nim compile --run -o:./exec/ad_select_server -r ad_select_server.nim
]#

import net, nativesockets, os, selectors # Import the packages required for select functions. 

var 
    server: Socket = newSocket()
    select: Selector[int] = newSelector[int]() #[ We define our selector. It requires a type too. 
    This would be the type of the data that we will associate in registerHandle method. ]#

server.setSockOpt(OptReuseAddr, true)
server.getFd().setBlocking(false)
server.bindAddr(Port(5555))
server.listen()
stdout.writeLine("Server: started. Listening to new connections on port 5555...")

select.registerHandle(server.getFd(), {Event.Read}, -1) #[ Register our server for read events. Note that read events on server socket 
would be incoming socket connections. Since we don’t have any data associated with our server, we’ll pass data as -1. ]#

var clients: seq[Socket] = @[]

while true:
  var results: seq[ReadyKey] = select.select(-1) #[ select returns a list of events along with 
  their file descriptors for which the specified event was triggered. ]#

  for result in results:
    if Event.Read in result.events: # We are only interested in read events.
      if result.fd.SocketHandle == server.getFd(): #[ The result received from select method contains the list of events that were captured 
      and the file descriptor fd for which the event was triggered. Though our fd is enough, we compare the underlying socket for that file descriptor. 
      If the one that triggered a read event was our server, it must mean that we recevied an incoming connection from a client. ]#
        var client: Socket = new(Socket)
        server.accept(client)

        client.getFd().setBlocking(false) # Since clients are also handled by select we’ll set them as non-blocking.

        select.registerHandle(client.getFd(), {Event.Read}, -1) #[ Just like our server, listen to read events of our client. 
        Read event on a connected socket indicates that a new message has been arrived waiting to be read. ]#

        clients.add(client)
        stdout.writeLine("Server: client connected")
      else: #[ If the read event was not triggered by server, then it must be client. 
      This means we received a message from a client which we have to broadcast to other clients. ]#
        
        var sourceClient: Socket = newSocket(result.fd.SocketHandle) #[ Working with higher-level socket abstractions is easier. 
        So, we convert our low-level socket from file descriptor in the result into a higher-level socket. ]#
        
        var message = sourceClient.recvLine()

        if message == "":
          var clientsToRemove: seq[int] = @[]

          for index, client in clients:
            if client.getFd() == sourceClient.getFd(): #[ We could have compared the two objects directly. \
            But since the newly created `sourceClient` object as a copy, it’s identity would be different. 
            This would cause an inequality even though the underlying sockets are same. ]#

              clientsToRemove.add(index)

          #[ If a client is disconnected, remove it from clients list and unregister events for that client. ]#
          for index in clientsToRemove:
            var client: Socket = clients[index]
            select.unregister(result.fd)
            clients.del(index)
            stdout.writeLine("Server: client disconnected")
        else:
          stdout.writeLine("Server: received from client: ", message)

          for client in clients:
            if client.getFd() == sourceClient.getFd():
              continue

            client.send(message & "\r\L")

select.close() #[ Like sockets, even select should be closed to signal the kernel that we are no longer listening to any events. 
This is a good practice to cleanly close everything before shutting down our program. ]#

server.close()