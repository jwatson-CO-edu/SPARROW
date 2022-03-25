#[
    Utils
    https://narimiran.github.io/nim-basics/#_modules
    User-defined module for Nim lessons
]#

#[ ##### Import Grammar #####

import myModule
import [firstFile, secondFile]
import mySubdir/thirdFile
import myOtherSubdir / [fourthFile, fifthFile]

]#

proc plus*(a, b: int): int = 
  return a + b

proc minus*(a, b: int): int = 
  return a - b