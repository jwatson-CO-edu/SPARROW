# nim c -d:release --hints:off --run -o:exec/closures -r closures.nim

proc closeness(str:string):proc(s:string):string=
    # https://forum.nim-lang.org/t/6789#42304
    let greeting = str
    return proc(s:string):string = greeting & s & '!'

let hello = closeness("Hello, ")
let hey = closeness("Hey, ")
echo hello( "Nim" )
echo hey( "there" )
assert hello("Nim") == "Hello, Nim!"
assert hey("there") == "Hey, there!"