import std/[random, strutils, strformat]
import lua/[minify, compile, tree]

proc build(input: string, compile: bool, minify: bool): string =
  result = treeBuild(input)

  if minify:
    result = minify(result)
  if compile:
    result = $compile(result)

proc laurus(output = "", input = ".", compile = false, minify = true) =
  randomize()

  let data = build(input, compile, minify)

  let filename = block:
    if output.isEmptyOrWhitespace:
      if compile: "result.bin"
      else: "result.lua"
    else:
      output

  echo &"writing to {filename}..."
  var fp = open(filename, fmWrite)
  fp.write(data)
  fp.close()

  echo "done!"

when isMainModule:
  import cligen; dispatch laurus