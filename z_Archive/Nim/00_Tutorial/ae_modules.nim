#[
    Lesson AE: Modules
    https://narimiran.github.io/nim-basics/#_modules
    nim c -d:release --hints:off --run -o:exec/ae_modules -r ae_modules.nim
]#

import strutils  # String utilities

let
  a = "My string with whitespace."
  b = '!'

echo a.split()        
echo a.toUpperAscii() 
echo b.repeat(5)  

import utils

echo plus(5, 10)          
echo minus(10, 5)