## nim c -d:release --hints:off --run -o:exec/aa_hello -r aa_hello.nim ##

# This is a comment
echo "What's your name? "
var name: string = readLine( stdin )
echo "Hi, ", name, "!"