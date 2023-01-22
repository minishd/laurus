import lua

const luaSrcDiet: cstring = staticRead("luasrcdiet.lua")

proc minify*(source: string): string =
  var L = newstate()

  openlibs(L)

  discard dostring(L, luaSrcDiet)

  pushstring(L, source)

  discard pcall(L, 1, 1, 0)

  result = $tostring(L, 1)

  close(L)