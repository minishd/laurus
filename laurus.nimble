# Package

version       = "0.1.0"
author        = "minish"
description   = "Merge a Lua source tree into one file"
license       = "MIT"
srcDir        = "src"
binDir        = "target"
bin           = @["laurus"]

# Dependencies

requires "nim >= 1.6.0"
requires "cligen >= 1.5.37"
