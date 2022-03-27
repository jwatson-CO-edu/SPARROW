# nim c -d:release --hints:off --run -o:exec/sequence_lists -r sequence_lists.nim

proc make_sequence( seqLen: int ): seq[int] = 
    # Return a sequence of length `seqLen`
    var rtnSeq: seq[int] = @[]
    for i in 1 .. seqLen:
        rtnSeq.add(i)
    return rtnSeq

echo make_sequence(5)