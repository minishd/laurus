# laurus
This tool merges a Lua source tree into one output file. It can also minify the output or compile it into Lua 5.1 bytecode. The code it generates is compatible with Lua versions 5.1 and above.

## Installation
This can be installed by cloning the Git repository and using `nimble install`.
```
git clone https://github.com/yccb/laurus.git
nimble install
```
Nim is required to compile laurus and Lua 5.1 libraries are required to run it.

## Usage
```
Usage:
  laurus [optional-params]
Options:
  -h, --help                     print this cligen-erated help
  --help-syntax                  advanced: prepend,plurals,..
  -o=, --output=  string  ""     set output
  -i=, --input=   string  "."    set input
  -c, --compile   bool    false  set compile
  -m, --minify    bool    true   set minify
```
By default results will be saved to `result.lua`, or `result.bin` if you chose to compile.
