import std/[asyncnet, asyncdispatch]
#[
    https://nim-lang.org/docs/asyncnet.html#examples-chat-server
    
    The async dispatcher implements the proactor pattern and also has an implementation of IOCP. 

    Async await transformation: This allows you to write asynchronous code in a synchronous style and works similar to C#'s await. 
    The transformation works by converting any async procedures into an iterator.

    This is all single threaded, fully non-blocking and does give you a lot of control.

    Secure Socket Layer(SSL) can be enabled by compiling with the -d:ssl flag.
]#

var clients {.threadvar.}: seq[AsyncSocket]

proc processClient(client: AsyncSocket) {.async.} =
  while true:
    
    let line = await client.recvLine() #[ Reads a line of data from socket. Returned future will complete once a full line is read or an error occurs.
    If a full line is read \r\L is not added to line, however if solely \r\L is read then line will be set to it.
    If the socket is disconnected, line will be set to "". If the socket is disconnected in the middle of a line (before \r\L is read) 
    then line will be set to "". The partial line will be lost. The maxLength parameter determines the maximum amount of characters that can be read. 
    The result is truncated after that. ]#
    
    if line.len == 0: break
    for c in clients:

      await c.send(line & "\c\L") #[ Sends data to socket. The returned future will complete once all data has been sent.  ]#

proc serve() {.async.} =
  clients = @[]
  
  var server = newAsyncSocket() #[ Creates a new asynchronous socket. This procedure will also create a brand new file descriptor for this socket. ]#
  
  server.setSockOpt(OptReuseAddr, true)
  
  server.bindAddr(Port(12345)) # Binds address:port to the socket.
  
  server.listen() #[ Marks socket as accepting connections. Backlog specifies the maximum length of the queue of pending connections. ]#
  
  while true:

    let client = await server.accept() #[ Accepts a new connection. 
    Returns a future containing the client socket corresponding to that connection. 
    The future will complete when the connection is successfully accepted. ]#
    
    clients.add client
    
    asyncCheck processClient(client)

asyncCheck serve()
runForever()