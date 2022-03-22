#[
    Lesson AH: References and Pointers
    https://nim-lang.org/docs/tut1.html#advanced-types-reference-and-pointer-types
    nim c -d:release --hints:off --run -o:exec/ah_ptr_ref -r ah_ptr_ref.nim
]#

#[### References ##### 
    Nim distinguishes between traced and untraced references. Untraced references are also called pointers. 
Traced references point to objects in a garbage-collected heap, 
untraced references point to manually allocated objects or objects elsewhere in memory. Thus untraced references are unsafe. 
However, for certain low-level operations (e.g. accessing the hardware), untraced references are necessary.

    The empty `[]` subscript notation can be used to de-refer a reference, meaning to retrieve the item the reference points to. 
The `.` (access a tuple/object field operator) and `[]` (array/string/sequence index operator) operators perform implicit dereferencing 
operations for reference types:  ]#

type
  Node = ref object
    le, ri: Node
    data: int

var n = Node(data: 9)
echo n.data
# no need to write n[].data; in fact n[].data is highly discouraged!


# FIXME: https://nim-lang.org/docs/tut1.html#advanced-types-procedural-type