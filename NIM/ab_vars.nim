## nim c -d:release --hints:off --run -o:exec/ab_vars -r ab_vars.nim ##

# One var of one type
var q: float

# Many vars of one type
var x, y: int # declares x and y to have the type ``int``

# Many vars of many types
var
  z, h: int
  # a comment can occur here too
  a, b, c: string