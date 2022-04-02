#[
    Doubly Linked List
    https://rosettacode.org/wiki/Doubly-linked_list/Traversal#Nim

    nim c -d:release --hints:off --run -o:exec/DLL -r DLL.nim
]# 


########## INIT ####################################################################################

import std/random # https://nim-lang.org/docs/random.html
# Call randomize() once to initialize the default random number generator.
# If this is not called, the same results will occur every time these
# examples are run.
randomize()


########## DLL TYPES ###############################################################################

type
    # List Types #

    DL_List[T] = object # Doubly-Linked List
        head, tail: Node[T]
        length: int

    Node[T] = ref object # DLL Node
        next, prev: Node[T]
        data: T



########## DLL FUNCTIONS ###########################################################################


proc initDLL[T](): DL_List[T] = discard 
    # Create a new list
 
proc newNode[T](data: T): Node[T] =
    # Create a new node and populate the data field
    new(result) # CONCEPT: Easy constructor
    result.data = data
 
proc prepend[T](l: var DL_List[T], n: Node[T]) =
    # Place a node at the head of the list
    l.length += 1
    n.next = l.head
    if l.head != nil: l.head.prev = n
    l.head = n
    if l.tail == nil: l.tail = n
 
proc append[T](l: var DL_List[T], n: Node[T]) =
    # Place a node at the end of the list
    l.length += 1
    n.next = nil
    n.prev = l.tail
    if l.tail != nil:
        l.tail.next = n
    l.tail = n
    if l.head == nil:
        l.head = n
 
proc insertAfter[T](l: var DL_List[T], r, n: Node[T]) =
    # Insert after the `r`eferenced node
    l.length += 1
    n.prev = r
    n.next = r.next
    n.next.prev = n
    r.next = n
    if r == l.tail: l.tail = n
 
proc remove[T](l: var DL_List[T], n: Node[T]) =
    # Remove the referenced node
    if l.length > 0:  l.length -= 1
    if n == l.tail: l.tail = n.prev
    if n == l.head: l.head = n.next
    if n.next != nil: n.next.prev = n.prev
    if n.prev != nil: n.prev.next = n.next
 
proc len[T](l: var DL_List[T]): int =
    # Return the number of elements in the list
    return l.length 

proc `$`[T](n: var Node[T]): string =
    # String representation of the Node
    result = "N["
    result.add($n.data)
    result.add("]")

proc `$`[T](l: var DL_List[T]): string =
    # String representation of the DLL
    result = ""
    var n = l.head
    while n != nil:
        if result.len > 0: result.add(" -> ")
        result.add($n.data)
        n = n.next
 
iterator traverseForward[T](l: DL_List[T]): T =
    # Iterator from head to tail
    var n = l.head
    while n != nil:
        yield n.data
        n = n.next

iterator iterNodes*[T](l: DL_List[T]): Node[T] =
    # Iterator from head to tail
    var n = l.head
    while n != nil:
        yield n
        n = n.next
 
iterator traverseBackward[T](l: DL_List[T]): T =
    # Iterator from tail to head
    var n = l.tail
    while n != nil:
        yield n.data
        n = n.prev

proc get_node_by_index*[T]( l: var DL_List[T], idx: int ): Node[T] =
    # Get the element at position `idx`, if it exists, otherwise return `nil`, O(n)
    var i: int = 0
    if idx > -1 and idx < len(l):
        for elem in iterNodes( l ):
            if i == idx:
                return elem
            else:
                i += 1
    return nil

proc get_value_by_index*[T]( l: var DL_List[T], idx: int ): T =
    # Get the element at position `idx`, if it exists, otherwise return `nil`, O(n)
    var i: int = 0
    if idx > -1 and idx < len(l):
        for elem in iterNodes( l ):
            if i == idx:
                return elem.data
            else:
                i += 1
    return -500

proc insert_node_after_index*[T]( l: var DL_List[T], idx: int, n: Node[T] ): bool {.discardable.} =
    # Find the node at `idx` and insert the given `n`ode after it, If successful, return true, else if DNE, return false
    var r = get_node_by_index[T]( l, idx )
    if r != nil:
        insertAfter( l, r, n )
        return true
    return false

proc remove_node_by_index*[T]( l: var DL_List[T], idx: int ): bool {.discardable.} =
    # Find the node at `idx` and remove it, If successful, return true, else if DNE, return false
    var n = get_node_by_index[T]( l, idx )
    if n != nil:
        remove( l, n )
        return true
    return false

proc choose_random_node*[T]( l: var DL_List[T] ): Node[T] =
    # Return a random node in the list
    return get_node_by_index( l, rand( l.len()-1 ) )

proc remove_random_node*[T]( l: var DL_List[T] ): void =
    # Return a random node in the list
    remove_node_by_index( l, rand( l.len()-1 ) )


########## BASIC TESTS #############################################################################
 
##### Original Tests #####

echo "Original Tests\n"

var dll = initDLL[int]()
var n = newNode(12)
var m = newNode(13)
var i = newNode(14)
var j = newNode(15)
dll.append(n)
dll.prepend(m)
dll.insertAfter(m, i)
dll.prepend(j)
dll.remove(m)
 
for i in dll.traverseForward(): # CONCEPT: Looping over an iterator
    echo "> ", i
 
for i in dll.traverseBackward():
    echo "< ", i

##### New Tests #####
echo "\nNew Tests\n"

var N = dll.get_node_by_index(2)
var D = dll.get_value_by_index(2)
echo "dll _______ = ", $dll
echo "dll[2] ____ = ", $N
echo "dll[2].data = ", $D
echo "Insert 13 after index 1 ..."
dll.insert_node_after_index( 1, newNode(13) )
echo "dll = ", $dll
echo "Delete node at index 3 ..."
dll.remove_node_by_index(3)
echo "dll = ", $dll
N = dll.choose_random_node()
echo "Random Node: ", $N
echo "Delete node at random index ..."
dll.remove_random_node()
echo "dll = ", $dll

########## HEAP TESTS ##############################################################################

let topNum:int = 5000

echo "Prepend numbers from the heap ..."
for i in 0..4:
    dll.prepend( newNode( rand(topNum) ) )
echo "dll = ", $dll

echo "Append numbers from the heap ..."
for i in 0..4:
    dll.append( newNode( rand(topNum) ) )
echo "dll = ", $dll
########## STRESS TESTS ##############################################################################

# FIXME:
    # NEW LIST
    # APPEND  500 RANDOM NUMBERS
    # PREPEND 500 RANDOM NUMBERS
    # REMOVE RANDOMLY UNTIL EMPTY
    # TIME ALL THREE OPERATIONS TOTAL AND AVERAGE