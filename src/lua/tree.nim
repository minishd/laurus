import std/[os, strutils, re]
import "merge.scf"

proc cleanPath(path: string): string =
  path
      .replace($DirSep, "/")
      .replace(re"^(\.\/*)", "")

proc treeBuild*(treePath: string): string =
  var files: seq[TreeFile]

  let originalDir = getCurrentDir()

  setCurrentDir(treePath)

  for fn in walkDirRec("."):
    var (dir, name, ext) = splitFile(fn)

    if ext != ".lua":
      continue
    
    var fp = open(fn, fmRead)

    var file = (
      name: cleanPath(dir & "/" & name),
      content: fp.readAll(),
      isIndex: cmpPaths(fn, "./index.lua") == 0,
    )

    fp.close()

    files &= file
  
  setCurrentDir(originalDir)
  
  result = mergeTree(files)