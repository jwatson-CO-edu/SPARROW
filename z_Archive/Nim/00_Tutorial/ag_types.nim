#[
    Lesson AG: Builtin and User-Defined Types
    https://nim-lang.org/docs/tut1.html#advanced-types-enumerations
    nim c -d:release --hints:off --run -o:exec/ag_types -r ag_types.nim
]#


##### Enum Types #####

type
    Direction = enum
        north, east, south, west

var x = south     # `x` is of type `Direction`; its value is `south`
echo x            # prints "south"



#[### Sets #####
    Sets can be constructed via the set constructor: {} is the empty set. 
The empty set is type compatible with any concrete set type. 
The constructor can also be used to include elements (and ranges of elements): ]#
type
  CharSet = set[char]
var
  y: CharSet
y = {'a'..'z', '0'..'9'} # This constructs a set that contains the
                         # letters from 'a' to 'z' and the digits
                         # from '0' to '9'


#[### "typedef" and Arrays ##### 
    The built-in len proc returns the array's length. low(a) returns the lowest valid index for the array a and high(a) the highest valid index.
]#
type # Equivalent to C `typedef`
  IntArray = array[0..5, int] # an array that is indexed with 0..5
  QuickArray = array[6, int]  # an array that is indexed with 0..5
var
  z: IntArray # Use of the new typename
z = [1, 2, 3, 4, 5, 6]
for i in low(z) .. high(z):
  echo z[i]

var
  a: IntArray
  b: QuickArray
a = [1, 2, 3, 4, 5, 6]
b = a
for i in low(a) .. high(a):
  echo a[i], b[i]

#[### Sequences ##### 
    Sequences are similar to arrays but of dynamic length which may change during runtime (like strings). 
Since sequences are resizable they are always allocated on the heap and garbage collected.
]#
var
  c: seq[int] # a reference to a sequence of integers
c = @[1, 2, 3, 4, 5, 6] # the @ turns the array into a sequence allocated on the heap

### Sequence Iteration ###
for value in @[3, 4, 5]:
  echo value
# --> 3
# --> 4
# --> 5

for i, value in @[3, 4, 5]:
  echo "index: ", $i, ", value:", $value
# --> index: 0, value:3
# --> index: 1, value:4
# --> index: 2, value:5


### Open Arrays ###
var
  fruits:   seq[string]       # reference to a sequence of strings that is initialized with '@[]'
  capitals: array[3, string]  # array of strings with a fixed size

capitals = ["New York", "London", "Berlin"]   # array 'capitals' allows assignment of only three elements
fruits.add("Banana")          # sequence 'fruits' is dynamically expandable during runtime
fruits.add("Mango")

proc openArraySize(oa: openArray[string]): int =
  oa.len

assert openArraySize(fruits) == 2     # procedure accepts a sequence as parameter
assert openArraySize(capitals) == 3   # but also an array type


#[### Objects (Structs) #####
An object is a value type, which means that when an object is assigned to a new variable all its components are copied as well.
]#

type
  Person = object
    name: string
    age: int

var person1 = Person(name: "Peter", age: 30)

echo person1.name # "Peter"
echo person1.age  # 30

var person2 = person1 # copy of person 1

person2.age += 14

echo person1.age # 30
echo person2.age # 44


# the order may be changed
let person3 = Person(age: 12, name: "Quentin")

# not every member needs to be specified
let person4 = Person(age: 3)
# unspecified members will be initialized with their default
# values. In this case it is the empty string.
doAssert person4.name == ""

# Object fields that should be visible from outside the defining module have to be marked with *.

type
  PublicPerson* = object # the type is visible from other modules
    name*: string  # the field of this type is visible from other modules
    age*: int


#[### Objects (Named Tuple) #####
Tuples are very much like what you have seen so far from objects. They are value types where the assignment operator copies each component. 
Unlike object types though, tuple-types are equivalent if they specify fields of the same type and of the same name in the same order.  ]#

type
  # type representing a person:
  # A person consists of a name and an age.
  PersonTuple = tuple
    name: string
    age: int
  
  # Alternative syntax for an equivalent type.
  PersonX = tuple[name: string, age: int]
  
  # anonymous field syntax
  PersonY = (string, int)

var
  person: PersonTuple
  personX: PersonX
  personY: PersonY

person = (name: "Peter", age: 30)
# Person and PersonX are equivalent
personX = person

# Create a tuple with anonymous fields:
personY = ("Peter", 30)

# A tuple with anonymous fields is compatible with a tuple that has
# field names.
person = personY
personY = person

# Usually used for short tuple initialization syntax
person = ("Peter", 30)

echo person.name # "Peter"
echo person.age  # 30

echo person[0] # "Peter"
echo person[1] # 30

# You don't need to declare tuples in a separate type section.
var building: tuple[street: string, number: int]
building = ("Rue del Percebe", 13)
echo building.street

# The following line does not compile, they are different tuples!
#person = building
# --> Error: type mismatch: got (tuple[street: string, number: int])
#     but expected 'Person'


### Unpacking ###

import std/os

let
  path = "usr/local/nimc.html"
  (dir, name, ext) = splitFile(path)
  baddir, badname, badext = splitFile(path)
echo dir      # outputs "usr/local"
echo name     # outputs "nimc"
echo ext      # outputs ".html"
# All the following output the same line:
# "(dir: usr/local, name: nimc, ext: .html)"
echo baddir
echo badname
echo badext


# Tuple unpacking is also supported in for-loops:

let d = [(10, 'a'), (20, 'b'), (30, 'c')]

for (x, c) in d:
  echo x
# This will output: 10; 20; 30

# Accessing the index is also possible:
for i, (x, c) in d:
  echo i, c
# This will output: 0a; 1b; 2c