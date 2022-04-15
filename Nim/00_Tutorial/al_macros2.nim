import algorithm, macros, sugar

type
  Foo = object
    x:int


var s = @[Foo(x:5),Foo(x:1),Foo(x:2)]

echo s

macro doit1(field:untyped):untyped =
  result = newStmtList(quote do:
    s.sorted(proc (f1, f2: Foo): int = cmp(f1.`field`,f2.`field`)))
  echo "doit1"
  echo result.treeRepr

macro doit2(field:untyped):untyped =
  result = newStmtList(quote do:
    proc myCmp(f1,f2:Foo):int = cmp(f1.`field`,f2.`field`)
    s.sorted(myCmp))
  echo "doit2"
  echo result.treeRepr

macro doit3(field:untyped):untyped =
  result = newStmtList(quote do:
    let myCmp = proc (f1,f2:Foo):int = cmp(f1.`field`,f2.`field`)
    s.sorted(myCmp))
  echo "doit3"
  echo result.treeRepr

macro doit4(field:untyped):untyped =
  result = newStmtList(quote do:
    s.sorted((f1,f2:Foo) => cmp(f1.`field`,f2.`field`)))
  echo "doit4"
  echo result.treeRepr

echo doit1(x)
echo doit2(x)
echo doit3(x)
echo doit4(x)