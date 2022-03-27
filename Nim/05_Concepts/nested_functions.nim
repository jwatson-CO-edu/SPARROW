# nim c -d:release --hints:off --run -o:exec/nested_functions -r nested_functions.nim

proc make_sequence( code: int ): seq[int] = 
    # Return a sequence of length `seqLen`
    var rtnSeq: seq[int] = @[]
    
    proc code_1( vec: var seq[int] ) = # Sequence arguments are IMMUTABLE unless the `var` keyword is used!
        # https://nim-by-example.github.io/seqs/
        for i in 1 .. 3:
            vec.add( int(i) )

    proc code_2( vec: var seq[int] ) = # Sequence arguments are IMMUTABLE unless the `var` keyword is used!
        # https://nim-by-example.github.io/seqs/
        for i in 4 .. 6:
            vec.add( int(i) )

    proc code_3( vec: var seq[int] ) = # Sequence arguments are IMMUTABLE unless the `var` keyword is used!
        # https://nim-by-example.github.io/seqs/
        for i in 7 .. 9:
            vec.add( int(i) )

    case code:
    of 1:
        code_1( rtnSeq )
    of 2:
        code_2( rtnSeq )
    of 3:
        code_3( rtnSeq )
    else:
        for i in 10 .. 25:
            rtnSeq.add(i)

    return rtnSeq


echo make_sequence( 1 )
echo make_sequence( 2 )
echo make_sequence( 3 )
echo make_sequence( 4 )