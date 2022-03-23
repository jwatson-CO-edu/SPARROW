#[
    Lesson AI: Object-Oriented Programming
    https://nim-lang.org/docs/tut2.html#object-oriented-programming
    nim c -d:release --hints:off --run -o:exec/ai_OOP -r ai_OOP.nim
]#

#[### Inheritance ##### 
    Inheritance in Nim is entirely optional. To enable inheritance with runtime type information the object needs to inherit from `RootObj`.

    Usually types with inheritance are also marked as ref types even though this isn't strictly enforced. 
To check at runtime if an object is of a certain type, the of operator can be used.

    Inheritance is done with the `object of` syntax. Multiple inheritance is currently not supported. 
If an object type has no suitable ancestor, `RootObj` can be used as its ancestor, but this is only a convention. 
Objects that have no ancestor are implicitly final. You can use the inheritable pragma to introduce new object roots apart from system.RootObj.

    Ref objects should be used whenever inheritance is used. It isn't strictly necessary, but with non-ref objects, 
assignments such as `let person: Person = Student(id: 123)` will truncate subclass fields.

    Note: Composition (has-a relation) is often preferable to inheritance (is-a relation) for simple code reuse. 
Since objects are value types in Nim, composition is as efficient as inheritance.
]#

type
  Person = ref object of RootObj
    name*: string  # the * means that `name` is accessible from other modules
    age: int       # no * means that the field is hidden from other modules
  
  Student = ref object of Person # Student inherits from Person
    id: int                      # with an id field

var
  student: Student
  person: Person
assert(student of Student) # is always true
# object construction:
person  = Person(name: "Gerbort", age: 500)
student = Student(name: "Anton", age: 5, id: 2)
echo person[]
echo student[]

#[ FIXME: https://nim-lang.org/docs/tut2.html#object-oriented-programming-mutually-recursive-types

]#