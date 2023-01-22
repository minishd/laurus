import lua

var bytecode {.threadvar.}: seq[byte]

proc writer(L: PState, p: pointer, sz: cint, ud: pointer): cint{.cdecl.} =
  var chunk = newSeq[byte](sz)
  copyMem(chunk[0].addr, p, sz)

  bytecode.add(chunk)

proc compile*(source: string): seq[byte] =
  bytecode = newSeq[byte]()

  var L = newstate()

  discard loadbuffer(L, source, source.len.cint, nil)
  discard dump(L, writer, nil)

  close(L)

  return bytecode