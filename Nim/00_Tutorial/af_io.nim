#[
    Lesson AF: Input / Output
    https://narimiran.github.io/nim-basics/#_reading_from_a_file
    nim c -d:release --hints:off --run -o:exec/af_io -r af_io.nim
]#

##### Reading Files #####

import strutils

let 
    entireFile = readFile("names.txt")
    people = entireFile.splitLines()    
echo entireFile  # prints the entire file
echo people



##### Parsing Input #####

echo "Please enter your year of birth:"
let 
    yearOfBirth = readLine(stdin).parseInt() 
    currYear    = 2022
    age         = currYear - yearOfBirth


echo "You were ", age, " years old in the year ", currYear