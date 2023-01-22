-- packed using svcompress v1

local _pack = {}
_pack.package_cache = {}
_pack.module_cache = {}
_pack.real_require = require

require = function(modname)
	local cached_package = _pack.package_cache[modname]
	local cached_module = _pack.module_cache[modname]

	if cached_package and not cached_module then
		_pack.module_cache[modname] = cached_package()
		cached_module = _pack.module_cache[modname]
	end
	
	if cached_module ~= nil then
		return cached_module
	else
		return _pack.real_require(modname)
	end
end;


_pack.package_cache['equiv'] = (function()
---------
-- Source and binary equivalency comparisons
--
-- **Notes:**
--
-- * Intended as an extra safety check for mission-critical code,
--   should give affirmative results if everything works.
-- * Heavy on load() and string.dump(), which may be slowish,
--   and may cause problems for cross-compiled applications.
-- * Optional detailed information dump is mainly for debugging,
--   reason being, if the two are not equivalent when they should be,
--   then some form of optimization has failed.
-- * source: IMPORTANT: TK_NAME not compared if opt-locals enabled.
-- * binary: IMPORTANT: Some shortcuts are taken with int and size_t
--   value reading -- if the functions break, then the binary chunk
--   is very large indeed.
-- * binary: There is a lack of diagnostic information when a compare
--   fails; you can use ChunkSpy and compare using visual diff.
----
local byte = string.byte
local dump = string.dump
local load = loadstring or load  --luacheck: ignore 113
local sub = string.sub

local M = {}

local is_realtoken = {          -- significant (grammar) tokens
  TK_KEYWORD = true,
  TK_NAME = true,
  TK_NUMBER = true,
  TK_STRING = true,
  TK_LSTRING = true,
  TK_OP = true,
  TK_EOS = true,
}

local option, llex, warn


--- The initialization function.
--
-- @tparam {[string]=bool,...} _option
-- @tparam luasrcdiet.llex _llex
-- @tparam table _warn
function M.init(_option, _llex, _warn)
  option = _option
  llex = _llex
  warn = _warn
end

--- Builds lists containing a 'normal' lexer stream.
--
-- @tparam string s The source code.
-- @treturn table
-- @treturn table
local function build_stream(s)
  local stok, sseminfo = llex.lex(s) -- source list (with whitespace elements)
  local tok, seminfo   -- processed list (real elements only)
    = {}, {}
  for i = 1, #stok do
    local t = stok[i]
    if is_realtoken[t] then
      tok[#tok + 1] = t
      seminfo[#seminfo + 1] = sseminfo[i]
    end
  end--for
  return tok, seminfo
end

-- Tests source (lexer stream) equivalence.
--
-- @tparam string z
-- @tparam string dat
function M.source(z, dat)

  -- Returns a dumped string for seminfo compares.
  local function dumpsem(s)
    local sf = load("return "..s, "z")
    if sf then
      return dump(sf)
    end
  end

  -- Marks and optionally reports non-equivalence.
  local function bork(msg)
    if option.DETAILS then print("SRCEQUIV: "..msg) end
    warn.SRC_EQUIV = true
  end

  -- Get lexer streams for both source strings, compare.
  local tok1, seminfo1 = build_stream(z)        -- original
  local tok2, seminfo2 = build_stream(dat)      -- compressed

  -- Compare shbang lines ignoring EOL.
  local sh1 = z:match("^(#[^\r\n]*)")
  local sh2 = dat:match("^(#[^\r\n]*)")
  if sh1 or sh2 then
    if not sh1 or not sh2 or sh1 ~= sh2 then
      bork("shbang lines different")
    end
  end

  -- Compare by simple count.
  if #tok1 ~= #tok2 then
    bork("count "..#tok1.." "..#tok2)
    return
  end

  -- Compare each element the best we can.
  for i = 1, #tok1 do
    local t1, t2 = tok1[i], tok2[i]
    local s1, s2 = seminfo1[i], seminfo2[i]
    if t1 ~= t2 then  -- by type
      bork("type ["..i.."] "..t1.." "..t2)
      break
    end
    if t1 == "TK_KEYWORD" or t1 == "TK_NAME" or t1 == "TK_OP" then
      if t1 == "TK_NAME" and option["opt-locals"] then
        -- can't compare identifiers of locals that are optimized
      elseif s1 ~= s2 then  -- by semantic info (simple)
        bork("seminfo ["..i.."] "..t1.." "..s1.." "..s2)
        break
      end
    elseif t1 == "TK_EOS" then
      -- no seminfo to compare
    else-- "TK_NUMBER" or "TK_STRING" or "TK_LSTRING"
      -- compare 'binary' form, so dump a function
      local s1b,s2b = dumpsem(s1), dumpsem(s2)
      if not s1b or not s2b or s1b ~= s2b then
        bork("seminfo ["..i.."] "..t1.." "..s1.." "..s2)
        break
      end
    end
  end--for

  -- Successful comparison if end is reached with no borks.
end

--- Tests binary chunk equivalence (only for PUC Lua 5.1).
--
-- @tparam string z
-- @tparam string dat
function M.binary(z, dat)
  local TNIL     = 0  --luacheck: ignore 211
  local TBOOLEAN = 1
  local TNUMBER  = 3
  local TSTRING  = 4

  -- sizes of data types
  local endian
  local sz_int
  local sz_sizet
  local sz_inst
  local sz_number
  local getint
  local getsizet

  -- Marks and optionally reports non-equivalence.
  local function bork(msg)
    if option.DETAILS then print("BINEQUIV: "..msg) end
    warn.BIN_EQUIV = true
  end

  -- Checks if bytes exist.
  local function ensure(c, sz)
    if c.i + sz - 1 > c.len then return end
    return true
  end

  -- Skips some bytes.
  local function skip(c, sz)
    if not sz then sz = 1 end
    c.i = c.i + sz
  end

  -- Returns a byte value.
  local function getbyte(c)
    local i = c.i
    if i > c.len then return end
    local d = sub(c.dat, i, i)
    c.i = i + 1
    return byte(d)
  end

  -- Return an int value (little-endian).
  local function getint_l(c)
    local n, scale = 0, 1
    if not ensure(c, sz_int) then return end
    for _ = 1, sz_int do
      n = n + scale * getbyte(c)
      scale = scale * 256
    end
    return n
  end

  -- Returns an int value (big-endian).
  local function getint_b(c)
    local n = 0
    if not ensure(c, sz_int) then return end
    for _ = 1, sz_int do
      n = n * 256 + getbyte(c)
    end
    return n
  end

  -- Returns a size_t value (little-endian).
  local function getsizet_l(c)
    local n, scale = 0, 1
    if not ensure(c, sz_sizet) then return end
    for _ = 1, sz_sizet do
      n = n + scale * getbyte(c)
      scale = scale * 256
    end
    return n
  end

  -- Returns a size_t value (big-endian).
  local function getsizet_b(c)
    local n = 0
    if not ensure(c, sz_sizet) then return end
    for _ = 1, sz_sizet do
      n = n * 256 + getbyte(c)
    end
    return n
  end

  -- Returns a block (as a string).
  local function getblock(c, sz)
    local i = c.i
    local j = i + sz - 1
    if j > c.len then return end
    local d = sub(c.dat, i, j)
    c.i = i + sz
    return d
  end

  -- Returns a string.
  local function getstring(c)
    local n = getsizet(c)
    if not n then return end
    if n == 0 then return "" end
    return getblock(c, n)
  end

  -- Compares byte value.
  local function goodbyte(c1, c2)
    local b1, b2 = getbyte(c1), getbyte(c2)
    if not b1 or not b2 or b1 ~= b2 then
      return
    end
    return b1
  end

  -- Compares byte value.
  local function badbyte(c1, c2)
    local b = goodbyte(c1, c2)
    if not b then return true end
  end

  -- Compares int value.
  local function goodint(c1, c2)
    local i1, i2 = getint(c1), getint(c2)
    if not i1 or not i2 or i1 ~= i2 then
      return
    end
    return i1
  end

  -- Recursively-called function to compare function prototypes.
  local function getfunc(c1, c2)
    -- source name (ignored)
    if not getstring(c1) or not getstring(c2) then
      bork("bad source name"); return
    end
    -- linedefined (ignored)
    if not getint(c1) or not getint(c2) then
      bork("bad linedefined"); return
    end
    -- lastlinedefined (ignored)
    if not getint(c1) or not getint(c2) then
      bork("bad lastlinedefined"); return
    end
    if not (ensure(c1, 4) and ensure(c2, 4)) then
      bork("prototype header broken")
    end
    -- nups (compared)
    if badbyte(c1, c2) then
      bork("bad nups"); return
    end
    -- numparams (compared)
    if badbyte(c1, c2) then
      bork("bad numparams"); return
    end
    -- is_vararg (compared)
    if badbyte(c1, c2) then
      bork("bad is_vararg"); return
    end
    -- maxstacksize (compared)
    if badbyte(c1, c2) then
      bork("bad maxstacksize"); return
    end
    -- code (compared)
    local ncode = goodint(c1, c2)
    if not ncode then
      bork("bad ncode"); return
    end
    local code1 = getblock(c1, ncode * sz_inst)
    local code2 = getblock(c2, ncode * sz_inst)
    if not code1 or not code2 or code1 ~= code2 then
      bork("bad code block"); return
    end
    -- constants (compared)
    local nconst = goodint(c1, c2)
    if not nconst then
      bork("bad nconst"); return
    end
    for _ = 1, nconst do
      local ctype = goodbyte(c1, c2)
      if not ctype then
        bork("bad const type"); return
      end
      if ctype == TBOOLEAN then
        if badbyte(c1, c2) then
          bork("bad boolean value"); return
        end
      elseif ctype == TNUMBER then
        local num1 = getblock(c1, sz_number)
        local num2 = getblock(c2, sz_number)
        if not num1 or not num2 or num1 ~= num2 then
          bork("bad number value"); return
        end
      elseif ctype == TSTRING then
        local str1 = getstring(c1)
        local str2 = getstring(c2)
        if not str1 or not str2 or str1 ~= str2 then
          bork("bad string value"); return
        end
      end
    end
    -- prototypes (compared recursively)
    local nproto = goodint(c1, c2)
    if not nproto then
      bork("bad nproto"); return
    end
    for _ = 1, nproto do
      if not getfunc(c1, c2) then
        bork("bad function prototype"); return
      end
    end
    -- debug information (ignored)
    -- lineinfo (ignored)
    local sizelineinfo1 = getint(c1)
    if not sizelineinfo1 then
      bork("bad sizelineinfo1"); return
    end
    local sizelineinfo2 = getint(c2)
    if not sizelineinfo2 then
      bork("bad sizelineinfo2"); return
    end
    if not getblock(c1, sizelineinfo1 * sz_int) then
      bork("bad lineinfo1"); return
    end
    if not getblock(c2, sizelineinfo2 * sz_int) then
      bork("bad lineinfo2"); return
    end
    -- locvars (ignored)
    local sizelocvars1 = getint(c1)
    if not sizelocvars1 then
      bork("bad sizelocvars1"); return
    end
    local sizelocvars2 = getint(c2)
    if not sizelocvars2 then
      bork("bad sizelocvars2"); return
    end
    for _ = 1, sizelocvars1 do
      if not getstring(c1) or not getint(c1) or not getint(c1) then
        bork("bad locvars1"); return
      end
    end
    for _ = 1, sizelocvars2 do
      if not getstring(c2) or not getint(c2) or not getint(c2) then
        bork("bad locvars2"); return
      end
    end
    -- upvalues (ignored)
    local sizeupvalues1 = getint(c1)
    if not sizeupvalues1 then
      bork("bad sizeupvalues1"); return
    end
    local sizeupvalues2 = getint(c2)
    if not sizeupvalues2 then
      bork("bad sizeupvalues2"); return
    end
    for _ = 1, sizeupvalues1 do
      if not getstring(c1) then bork("bad upvalues1"); return end
    end
    for _ = 1, sizeupvalues2 do
      if not getstring(c2) then bork("bad upvalues2"); return end
    end
    return true
  end

  -- Removes shbang line so that load runs.
  local function zap_shbang(s)
    local shbang = s:match("^(#[^\r\n]*\r?\n?)")
    if shbang then                      -- cut out shbang
      s = sub(s, #shbang + 1)
    end
    return s
  end

  -- Attempt to compile, then dump to get binary chunk string.
  local cz = load(zap_shbang(z), "z")
  if not cz then
    bork("failed to compile original sources for binary chunk comparison")
    return
  end

  local cdat = load(zap_shbang(dat), "z")
  if not cdat then
    bork("failed to compile compressed result for binary chunk comparison")
  end

  -- if load() works, dump assuming string.dump() is error-free
  local c1 = { i = 1, dat = dump(cz) }
  c1.len = #c1.dat

  local c2 = { i = 1, dat = dump(cdat) }
  c2.len = #c2.dat

  -- Parse binary chunks to verify equivalence.
  -- * For headers, handle sizes to allow a degree of flexibility.
  -- * Assume a valid binary chunk is generated, since it was not
  --   generated via external means.
  if not (ensure(c1, 12) and ensure(c2, 12)) then
    bork("header broken")
  end
  skip(c1, 6)                   -- skip signature(4), version, format
  endian    = getbyte(c1)       -- 1 = little endian
  sz_int    = getbyte(c1)       -- get data type sizes
  sz_sizet  = getbyte(c1)
  sz_inst   = getbyte(c1)
  sz_number = getbyte(c1)
  skip(c1)                      -- skip integral flag
  skip(c2, 12)                  -- skip other header (assume similar)

  if endian == 1 then           -- set for endian sensitive data we need
    getint   = getint_l
    getsizet = getsizet_l
  else
    getint   = getint_b
    getsizet = getsizet_b
  end
  getfunc(c1, c2)               -- get prototype at root

  if c1.i ~= c1.len + 1 then
    bork("inconsistent binary chunk1"); return
  elseif c2.i ~= c2.len + 1 then
    bork("inconsistent binary chunk2"); return
  end

  -- Successful comparison if end is reached with no borks.
end

return M

end);

_pack.package_cache['optparser'] = (function()
---------
-- This module does parser-based optimizations.
--
-- **Notes:**
--
-- * The processing load is quite significant, but since this is an
--   off-line text processor, I believe we can wait a few seconds.
-- * TODO: Might process "local a,a,a" wrongly... need tests!
-- * TODO: Remove position handling if overlapped locals (rem < 0)
--   needs more study, to check behaviour.
-- * TODO: There are probably better ways to do allocation, e.g. by
--   choosing better methods to sort and pick locals...
-- * TODO: We don't need 53*63 two-letter identifiers; we can make
--   do with significantly less depending on how many that are really
--   needed and improve entropy; e.g. 13 needed -> choose 4*4 instead.
----
local byte = string.byte
local char = string.char
local concat = table.concat
local fmt = string.format
local pairs = pairs
local rep = string.rep
local sort = table.sort
local sub = string.sub


local M = {}

-- Letter frequencies for reducing symbol entropy (fixed version)
-- * Might help a wee bit when the output file is compressed
-- * See Wikipedia: http://en.wikipedia.org/wiki/Letter_frequencies
-- * We use letter frequencies according to a Linotype keyboard, plus
--   the underscore, and both lower case and upper case letters.
-- * The arrangement below (LC, underscore, %d, UC) is arbitrary.
-- * This is certainly not optimal, but is quick-and-dirty and the
--   process has no significant overhead
local LETTERS = "etaoinshrdlucmfwypvbgkqjxz_ETAOINSHRDLUCMFWYPVBGKQJXZ"
local ALPHANUM = "etaoinshrdlucmfwypvbgkqjxz_0123456789ETAOINSHRDLUCMFWYPVBGKQJXZ"

-- Names or identifiers that must be skipped.
-- (The first two lines are for keywords.)
local SKIP_NAME = {}
for v in ([[
and break do else elseif end false for function if in
local nil not or repeat return then true until while
self _ENV]]):gmatch("%S+") do
  SKIP_NAME[v] = true
end


local toklist, seminfolist,             -- token lists (lexer output)
      tokpar, seminfopar, xrefpar,      -- token lists (parser output)
      globalinfo, localinfo,            -- variable information tables
      statinfo,                         -- statment type table
      globaluniq, localuniq,            -- unique name tables
      var_new,                          -- index of new variable names
      varlist                           -- list of output variables

--- Preprocesses information table to get lists of unique names.
--
-- @tparam {table,...} infotable
-- @treturn table
local function preprocess(infotable)
  local uniqtable = {}
  for i = 1, #infotable do              -- enumerate info table
    local obj = infotable[i]
    local name = obj.name

    if not uniqtable[name] then         -- not found, start an entry
      uniqtable[name] = {
        decl = 0, token = 0, size = 0,
      }
    end

    local uniq = uniqtable[name]        -- count declarations, tokens, size
    uniq.decl = uniq.decl + 1
    local xref = obj.xref
    local xcount = #xref
    uniq.token = uniq.token + xcount
    uniq.size = uniq.size + xcount * #name

    if obj.decl then            -- if local table, create first,last pairs
      obj.id = i
      obj.xcount = xcount
      if xcount > 1 then        -- if ==1, means local never accessed
        obj.first = xref[2]
        obj.last = xref[xcount]
      end

    else                        -- if global table, add a back ref
      uniq.id = i
    end

  end--for
  return uniqtable
end

--- Calculates actual symbol frequencies, in order to reduce entropy.
--
-- * This may help further reduce the size of compressed sources.
-- * Note that since parsing optimizations is put before lexing
--   optimizations, the frequency table is not exact!
-- * Yes, this will miss --keep block comments too...
--
-- @tparam table option
local function recalc_for_entropy(option)
  -- table of token classes to accept in calculating symbol frequency
  local ACCEPT = {
    TK_KEYWORD = true, TK_NAME = true, TK_NUMBER = true,
    TK_STRING = true, TK_LSTRING = true,
  }
  if not option["opt-comments"] then
    ACCEPT.TK_COMMENT = true
    ACCEPT.TK_LCOMMENT = true
  end

  -- Create a new table and remove any original locals by filtering.
  local filtered = {}
  for i = 1, #toklist do
    filtered[i] = seminfolist[i]
  end
  for i = 1, #localinfo do              -- enumerate local info table
    local obj = localinfo[i]
    local xref = obj.xref
    for j = 1, obj.xcount do
      local p = xref[j]
      filtered[p] = ""                  -- remove locals
    end
  end

  local freq = {}                       -- reset symbol frequency table
  for i = 0, 255 do freq[i] = 0 end
  for i = 1, #toklist do                -- gather symbol frequency
    local tok, info = toklist[i], filtered[i]
    if ACCEPT[tok] then
      for j = 1, #info do
        local c = byte(info, j)
        freq[c] = freq[c] + 1
      end
    end--if
  end--for

  -- Re-sorts symbols according to actual frequencies.
  --
  -- @tparam string symbols
  -- @treturn string
  local function resort(symbols)
    local symlist = {}
    for i = 1, #symbols do              -- prepare table to sort
      local c = byte(symbols, i)
      symlist[i] = { c = c, freq = freq[c], }
    end
    sort(symlist, function(v1, v2)  -- sort selected symbols
        return v1.freq > v2.freq
      end)
    local charlist = {}                 -- reconstitute the string
    for i = 1, #symlist do
      charlist[i] = char(symlist[i].c)
    end
    return concat(charlist)
  end

  LETTERS = resort(LETTERS)             -- change letter arrangement
  ALPHANUM = resort(ALPHANUM)
end

--- Returns a string containing a new local variable name to use, and
-- a flag indicating whether it collides with a global variable.
--
-- Trapping keywords and other names like 'self' is done elsewhere.
--
-- @treturn string A new local variable name.
-- @treturn bool Whether the name collides with a global variable.
local function new_var_name()
  local var
  local cletters, calphanum = #LETTERS, #ALPHANUM
  local v = var_new
  if v < cletters then                  -- single char
    v = v + 1
    var = sub(LETTERS, v, v)
  else                                  -- longer names
    local range, sz = cletters, 1       -- calculate # chars fit
    repeat
      v = v - range
      range = range * calphanum
      sz = sz + 1
    until range > v
    local n = v % cletters              -- left side cycles faster
    v = (v - n) / cletters              -- do first char first
    n = n + 1
    var = sub(LETTERS, n, n)
    while sz > 1 do
      local m = v % calphanum
      v = (v - m) / calphanum
      m = m + 1
      var = var..sub(ALPHANUM, m, m)
      sz = sz - 1
    end
  end
  var_new = var_new + 1
  return var, globaluniq[var] ~= nil
end

--- Calculates and prints some statistics.
--
-- Note: probably better in main source, put here for now.
--
-- @tparam table globaluniq
-- @tparam table localuniq
-- @tparam table afteruniq
-- @tparam table option
local function stats_summary(globaluniq, localuniq, afteruniq, option)  --luacheck: ignore 431
  local print = M.print or print
  local opt_details = option.DETAILS
  if option.QUIET then return end

  local uniq_g , uniq_li, uniq_lo = 0, 0, 0
  local decl_g, decl_li, decl_lo = 0, 0, 0
  local token_g, token_li, token_lo = 0, 0, 0
  local size_g, size_li, size_lo = 0, 0, 0

  local function avg(c, l)              -- safe average function
    if c == 0 then return 0 end
    return l / c
  end

  -- Collect statistics (Note: globals do not have declarations!)
  for _, uniq in pairs(globaluniq) do
    uniq_g = uniq_g + 1
    token_g = token_g + uniq.token
    size_g = size_g + uniq.size
  end
  for _, uniq in pairs(localuniq) do
    uniq_li = uniq_li + 1
    decl_li = decl_li + uniq.decl
    token_li = token_li + uniq.token
    size_li = size_li + uniq.size
  end
  for _, uniq in pairs(afteruniq) do
    uniq_lo = uniq_lo + 1
    decl_lo = decl_lo + uniq.decl
    token_lo = token_lo + uniq.token
    size_lo = size_lo + uniq.size
  end
  local uniq_ti = uniq_g + uniq_li
  local decl_ti = decl_g + decl_li
  local token_ti = token_g + token_li
  local size_ti = size_g + size_li
  local uniq_to = uniq_g + uniq_lo
  local decl_to = decl_g + decl_lo
  local token_to = token_g + token_lo
  local size_to = size_g + size_lo

  -- Detailed stats: global list
  if opt_details then
    local sorted = {} -- sort table of unique global names by size
    for name, uniq in pairs(globaluniq) do
      uniq.name = name
      sorted[#sorted + 1] = uniq
    end
    sort(sorted, function(v1, v2)
        return v1.size > v2.size
      end)

    do
      local tabf1, tabf2 = "%8s%8s%10s  %s", "%8d%8d%10.2f  %s"
      local hl = rep("-", 44)
      print("*** global variable list (sorted by size) ***\n"..hl)
      print(fmt(tabf1, "Token",  "Input", "Input", "Global"))
      print(fmt(tabf1, "Count", "Bytes", "Average", "Name"))
      print(hl)
      for i = 1, #sorted do
        local uniq = sorted[i]
        print(fmt(tabf2, uniq.token, uniq.size, avg(uniq.token, uniq.size), uniq.name))
      end
      print(hl)
      print(fmt(tabf2, token_g, size_g, avg(token_g, size_g), "TOTAL"))
      print(hl.."\n")
    end

    -- Detailed stats: local list
    do
      local tabf1, tabf2 = "%8s%8s%8s%10s%8s%10s  %s", "%8d%8d%8d%10.2f%8d%10.2f  %s"
      local hl = rep("-", 70)
      print("*** local variable list (sorted by allocation order) ***\n"..hl)
      print(fmt(tabf1, "Decl.", "Token",  "Input", "Input", "Output", "Output", "Global"))
      print(fmt(tabf1, "Count", "Count", "Bytes", "Average", "Bytes", "Average", "Name"))
      print(hl)
      for i = 1, #varlist do  -- iterate according to order assigned
        local name = varlist[i]
        local uniq = afteruniq[name]
        local old_t, old_s = 0, 0
        for j = 1, #localinfo do  -- find corresponding old names and calculate
          local obj = localinfo[j]
          if obj.name == name then
            old_t = old_t + obj.xcount
            old_s = old_s + obj.xcount * #obj.oldname
          end
        end
        print(fmt(tabf2, uniq.decl, uniq.token, old_s, avg(old_t, old_s),
                  uniq.size, avg(uniq.token, uniq.size), name))
      end
      print(hl)
      print(fmt(tabf2, decl_lo, token_lo, size_li, avg(token_li, size_li),
                size_lo, avg(token_lo, size_lo), "TOTAL"))
      print(hl.."\n")
    end
  end--if opt_details

  -- Display output
  do
    local tabf1, tabf2 = "%-16s%8s%8s%8s%8s%10s", "%-16s%8d%8d%8d%8d%10.2f"
    local hl = rep("-", 58)
    print("*** local variable optimization summary ***\n"..hl)
    print(fmt(tabf1, "Variable",  "Unique", "Decl.", "Token", "Size", "Average"))
    print(fmt(tabf1, "Types", "Names", "Count", "Count", "Bytes", "Bytes"))
    print(hl)
    print(fmt(tabf2, "Global", uniq_g, decl_g, token_g, size_g, avg(token_g, size_g)))
    print(hl)
    print(fmt(tabf2, "Local (in)", uniq_li, decl_li, token_li, size_li, avg(token_li, size_li)))
    print(fmt(tabf2, "TOTAL (in)", uniq_ti, decl_ti, token_ti, size_ti, avg(token_ti, size_ti)))
    print(hl)
    print(fmt(tabf2, "Local (out)", uniq_lo, decl_lo, token_lo, size_lo, avg(token_lo, size_lo)))
    print(fmt(tabf2, "TOTAL (out)", uniq_to, decl_to, token_to, size_to, avg(token_to, size_to)))
    print(hl.."\n")
  end
end

--- Does experimental optimization for f("string") statements.
--
-- It's safe to delete parentheses without adding whitespace, as both
-- kinds of strings can abut with anything else.
local function optimize_func1()

  local function is_strcall(j)          -- find f("string") pattern
    local t1 = tokpar[j + 1] or ""
    local t2 = tokpar[j + 2] or ""
    local t3 = tokpar[j + 3] or ""
    if t1 == "(" and t2 == "<string>" and t3 == ")" then
      return true
    end
  end

  local del_list = {}           -- scan for function pattern,
  local i = 1                   -- tokens to be deleted are marked
  while i <= #tokpar do
    local id = statinfo[i]
    if id == "call" and is_strcall(i) then  -- found & mark ()
      del_list[i + 1] = true    -- '('
      del_list[i + 3] = true    -- ')'
      i = i + 3
    end
    i = i + 1
  end

  -- Delete a token and adjust all relevant tables.
  -- * Currently invalidates globalinfo and localinfo (not updated),
  --   so any other optimization is done after processing locals
  --   (of course, we can also lex the source data again...).
  -- * Faster one-pass token deletion.
  local del_list2 = {}
  do
    local i, dst, idend = 1, 1, #tokpar
    while dst <= idend do         -- process parser tables
      if del_list[i] then         -- found a token to delete?
        del_list2[xrefpar[i]] = true
        i = i + 1
      end
      if i > dst then
        if i <= idend then        -- shift table items lower
          tokpar[dst] = tokpar[i]
          seminfopar[dst] = seminfopar[i]
          xrefpar[dst] = xrefpar[i] - (i - dst)
          statinfo[dst] = statinfo[i]
        else                      -- nil out excess entries
          tokpar[dst] = nil
          seminfopar[dst] = nil
          xrefpar[dst] = nil
          statinfo[dst] = nil
        end
      end
      i = i + 1
      dst = dst + 1
    end
  end

  do
    local i, dst, idend = 1, 1, #toklist
    while dst <= idend do         -- process lexer tables
      if del_list2[i] then        -- found a token to delete?
        i = i + 1
      end
      if i > dst then
        if i <= idend then        -- shift table items lower
          toklist[dst] = toklist[i]
          seminfolist[dst] = seminfolist[i]
        else                      -- nil out excess entries
          toklist[dst] = nil
          seminfolist[dst] = nil
        end
      end
      i = i + 1
      dst = dst + 1
    end
  end
end

--- Does local variable optimization.
--
-- @tparam {[string]=bool,...} option
local function optimize_locals(option)
  var_new = 0                           -- reset variable name allocator
  varlist = {}

  -- Preprocess global/local tables, handle entropy reduction.
  globaluniq = preprocess(globalinfo)
  localuniq = preprocess(localinfo)
  if option["opt-entropy"] then         -- for entropy improvement
    recalc_for_entropy(option)
  end

  -- Build initial declared object table, then sort according to
  -- token count, this might help assign more tokens to more common
  -- variable names such as 'e' thus possibly reducing entropy.
  -- * An object knows its localinfo index via its 'id' field.
  -- * Special handling for "self" and "_ENV" special local (parameter) here.
  local object = {}
  for i = 1, #localinfo do
    object[i] = localinfo[i]
  end
  sort(object, function(v1, v2)  -- sort largest first
      return v1.xcount > v2.xcount
    end)

  -- The special "self" and "_ENV" function parameters must be preserved.
  -- * The allocator below will never use "self", so it is safe to
  --   keep those implicit declarations as-is.
  local temp, j, used_specials = {}, 1, {}
  for i = 1, #object do
    local obj = object[i]
    if not obj.is_special then
      temp[j] = obj
      j = j + 1
    else
      used_specials[#used_specials + 1] = obj.name
    end
  end
  object = temp

  -- A simple first-come first-served heuristic name allocator,
  -- note that this is in no way optimal...
  -- * Each object is a local variable declaration plus existence.
  -- * The aim is to assign short names to as many tokens as possible,
  --   so the following tries to maximize name reuse.
  -- * Note that we preserve sort order.
  local nobject = #object
  while nobject > 0 do
    local varname, gcollide
    repeat
      varname, gcollide = new_var_name()  -- collect a variable name
    until not SKIP_NAME[varname]          -- skip all special names
    varlist[#varlist + 1] = varname       -- keep a list
    local oleft = nobject

    -- If variable name collides with an existing global, the name
    -- cannot be used by a local when the name is accessed as a global
    -- during which the local is alive (between 'act' to 'rem'), so
    -- we drop objects that collides with the corresponding global.
    if gcollide then
      -- find the xref table of the global
      local gref = globalinfo[globaluniq[varname].id].xref
      local ngref = #gref
      -- enumerate for all current objects; all are valid at this point
      for i = 1, nobject do
        local obj = object[i]
        local act, rem = obj.act, obj.rem  -- 'live' range of local
        -- if rem < 0, it is a -id to a local that had the same name
        -- so follow rem to extend it; does this make sense?
        while rem < 0 do
          rem = localinfo[-rem].rem
        end
        local drop
        for j = 1, ngref do
          local p = gref[j]
          if p >= act and p <= rem then drop = true end  -- in range?
        end
        if drop then
          obj.skip = true
          oleft = oleft - 1
        end
      end--for
    end--if gcollide

    -- Now the first unassigned local (since it's sorted) will be the
    -- one with the most tokens to rename, so we set this one and then
    -- eliminate all others that collides, then any locals that left
    -- can then reuse the same variable name; this is repeated until
    -- all local declaration that can use this name is assigned.
    --
    -- The criteria for local-local reuse/collision is:
    --   A is the local with a name already assigned
    --   B is the unassigned local under consideration
    --   => anytime A is accessed, it cannot be when B is 'live'
    --   => to speed up things, we have first/last accesses noted
    while oleft > 0 do
      local i = 1
      while object[i].skip do  -- scan for first object
        i = i + 1
      end

      -- First object is free for assignment of the variable name
      -- [first,last] gives the access range for collision checking.
      oleft = oleft - 1
      local obja = object[i]
      i = i + 1
      obja.newname = varname
      obja.skip = true
      obja.done = true
      local first, last = obja.first, obja.last
      local xref = obja.xref

      -- Then, scan all the rest and drop those colliding.
      -- If A was never accessed then it'll never collide with anything
      -- otherwise trivial skip if:
      -- * B was activated after A's last access (last < act),
      -- * B was removed before A's first access (first > rem),
      -- if not, see detailed skip below...
      if first and oleft > 0 then  -- must have at least 1 access
        local scanleft = oleft
        while scanleft > 0 do
          while object[i].skip do  -- next valid object
            i = i + 1
          end
          scanleft = scanleft - 1
          local objb = object[i]
          i = i + 1
          local act, rem = objb.act, objb.rem  -- live range of B
          -- if rem < 0, extend range of rem thru' following local
          while rem < 0 do
            rem = localinfo[-rem].rem
          end

          if not(last < act or first > rem) then  -- possible collision

            -- B is activated later than A or at the same statement,
            -- this means for no collision, A cannot be accessed when B
            -- is alive, since B overrides A (or is a peer).
            if act >= obja.act then
              for j = 1, obja.xcount do  -- ... then check every access
                local p = xref[j]
                if p >= act and p <= rem then  -- A accessed when B live!
                  oleft = oleft - 1
                  objb.skip = true
                  break
                end
              end--for

            -- A is activated later than B, this means for no collision,
            -- A's access is okay since it overrides B, but B's last
            -- access need to be earlier than A's activation time.
            else
              if objb.last and objb.last >= obja.act then
                oleft = oleft - 1
                objb.skip = true
              end
            end
          end

          if oleft == 0 then break end
        end
      end--if first

    end--while

    -- After assigning all possible locals to one variable name, the
    -- unassigned locals/objects have the skip field reset and the table
    -- is compacted, to hopefully reduce iteration time.
    local temp, j = {}, 1
    for i = 1, nobject do
      local obj = object[i]
      if not obj.done then
        obj.skip = false
        temp[j] = obj
        j = j + 1
      end
    end
    object = temp  -- new compacted object table
    nobject = #object  -- objects left to process

  end--while

  -- After assigning all locals with new variable names, we can
  -- patch in the new names, and reprocess to get 'after' stats.
  for i = 1, #localinfo do  -- enumerate all locals
    local obj = localinfo[i]
    local xref = obj.xref
    if obj.newname then                 -- if got new name, patch it in
      for j = 1, obj.xcount do
        local p = xref[j]               -- xrefs indexes the token list
        seminfolist[p] = obj.newname
      end
      obj.name, obj.oldname             -- adjust names
        = obj.newname, obj.name
    else
      obj.oldname = obj.name            -- for cases like 'self'
    end
  end

  -- Deal with statistics output.
  for _, name in ipairs(used_specials) do
    varlist[#varlist + 1] = name
  end
  local afteruniq = preprocess(localinfo)
  stats_summary(globaluniq, localuniq, afteruniq, option)
end

--- The main entry point.
--
-- @tparam table option
-- @tparam {string,...} _toklist
-- @tparam {string,...} _seminfolist
-- @tparam table xinfo
function M.optimize(option, _toklist, _seminfolist, xinfo)
  -- set tables
  toklist, seminfolist                  -- from lexer
    = _toklist, _seminfolist
  tokpar, seminfopar, xrefpar           -- from parser
    = xinfo.toklist, xinfo.seminfolist, xinfo.xreflist
  globalinfo, localinfo, statinfo       -- from parser
    = xinfo.globalinfo, xinfo.localinfo, xinfo.statinfo

  -- Optimize locals.
  if option["opt-locals"] then
    optimize_locals(option)
  end

  -- Other optimizations.
  if option["opt-experimental"] then    -- experimental
    optimize_func1()
    -- WARNING globalinfo and localinfo now invalidated!
  end
end

return M

end);

_pack.package_cache['llex'] = (function()
---------
-- Lua 5.1+ lexical analyzer written in Lua.
--
-- This file is part of LuaSrcDiet, based on Yueliang material.
--
-- **Notes:**
--
-- * This is a version of the native 5.1.x lexer from Yueliang 0.4.0,
--   with significant modifications to handle LuaSrcDiet's needs:
--   (1) llex.error is an optional error function handler,
--   (2) seminfo for strings include their delimiters and no
--       translation operations are performed on them.
-- * ADDED shbang handling has been added to support executable scripts.
-- * NO localized decimal point replacement magic.
-- * NO limit to number of lines.
-- * NO support for compatible long strings (LUA\_COMPAT_LSTR).
-- * Added goto keyword and double-colon operator (Lua 5.2+).
----
local find = string.find
local fmt = string.format
local match = string.match
local sub = string.sub
local tonumber = tonumber

local M = {}

local kw = {}
for v in ([[
and break do else elseif end false for function goto if in
local nil not or repeat return then true until while]]):gmatch("%S+") do
  kw[v] = true
end

local z,                -- source stream
      sourceid,         -- name of source
      I,                -- position of lexer
      buff,             -- buffer for strings
      ln,               -- line number
      tok,              -- lexed token list
      seminfo,          -- lexed semantic information list
      tokln             -- line numbers for messages


--- Adds information to token listing.
--
-- @tparam string token
-- @tparam string info
local function addtoken(token, info)
  local i = #tok + 1
  tok[i] = token
  seminfo[i] = info
  tokln[i] = ln
end

--- Handles line number incrementation and end-of-line characters.
--
-- @tparam int i Position of lexer in the source stream.
-- @tparam bool is_tok
-- @treturn int
local function inclinenumber(i, is_tok)
  local old = sub(z, i, i)
  i = i + 1  -- skip '\n' or '\r'
  local c = sub(z, i, i)
  if (c == "\n" or c == "\r") and (c ~= old) then
    i = i + 1  -- skip '\n\r' or '\r\n'
    old = old..c
  end
  if is_tok then addtoken("TK_EOL", old) end
  ln = ln + 1
  I = i
  return i
end

--- Returns a chunk name or id, no truncation for long names.
--
-- @treturn string
local function chunkid()
  if sourceid and match(sourceid, "^[=@]") then
    return sub(sourceid, 2)  -- remove first char
  end
  return "[string]"
end

--- Formats error message and throws error.
--
-- A simplified version, does not report what token was responsible.
--
-- @tparam string s
-- @tparam int line The line number.
-- @raise
local function errorline(s, line)
  local e = M.error or error
  e(fmt("%s:%d: %s", chunkid(), line or ln, s))
end

--- Counts separators (`="` in a long string delimiter.
--
-- @tparam int i Position of lexer in the source stream.
-- @treturn int
local function skip_sep(i)
  local s = sub(z, i, i)
  i = i + 1
  local count = #match(z, "=*", i)
  i = i + count
  I = i
  return (sub(z, i, i) == s) and count or (-count) - 1
end

--- Reads a long string or long comment.
--
-- @tparam bool is_str
-- @tparam string sep
-- @treturn string
-- @raise if unfinished long string or comment.
local function read_long_string(is_str, sep)
  local i = I + 1  -- skip 2nd '['
  local c = sub(z, i, i)
  if c == "\r" or c == "\n" then  -- string starts with a newline?
    i = inclinenumber(i)  -- skip it
  end
  while true do
    local p, _, r = find(z, "([\r\n%]])", i) -- (long range match)
    if not p then
      errorline(is_str and "unfinished long string" or
                "unfinished long comment")
    end
    i = p
    if r == "]" then                    -- delimiter test
      if skip_sep(i) == sep then
        buff = sub(z, buff, I)
        I = I + 1  -- skip 2nd ']'
        return buff
      end
      i = I
    else                                -- newline
      buff = buff.."\n"
      i = inclinenumber(i)
    end
  end--while
end

--- Reads a string.
--
-- @tparam string del The delimiter.
-- @treturn string
-- @raise if unfinished string or too large escape sequence.
local function read_string(del)
  local i = I
  while true do
    local p, _, r = find(z, "([\n\r\\\"\'])", i) -- (long range match)
    if p then
      if r == "\n" or r == "\r" then
        errorline("unfinished string")
      end
      i = p
      if r == "\\" then                         -- handle escapes
        i = i + 1
        r = sub(z, i, i)
        if r == "" then break end -- (EOZ error)
        p = find("abfnrtv\n\r", r, 1, true)

        if p then                               -- special escapes
          if p > 7 then
            i = inclinenumber(i)
          else
            i = i + 1
          end

        elseif find(r, "%D") then               -- other non-digits
          i = i + 1

        else                                    -- \xxx sequence
          local _, q, s = find(z, "^(%d%d?%d?)", i)
          i = q + 1
          if s + 1 > 256 then -- UCHAR_MAX
            errorline("escape sequence too large")
          end

        end--if p
      else
        i = i + 1
        if r == del then                        -- ending delimiter
          I = i
          return sub(z, buff, i - 1)            -- return string
        end
      end--if r
    else
      break -- (error)
    end--if p
  end--while
  errorline("unfinished string")
end


--- Initializes lexer for given source _z and source name _sourceid.
--
-- @tparam string _z The source code.
-- @tparam string _sourceid Name of the source.
local function init(_z, _sourceid)
  z = _z                        -- source
  sourceid = _sourceid          -- name of source
  I = 1                         -- lexer's position in source
  ln = 1                        -- line number
  tok = {}                      -- lexed token list*
  seminfo = {}                  -- lexed semantic information list*
  tokln = {}                    -- line numbers for messages*

  -- Initial processing (shbang handling).
  local p, _, q, r = find(z, "^(#[^\r\n]*)(\r?\n?)")
  if p then                             -- skip first line
    I = I + #q
    addtoken("TK_COMMENT", q)
    if #r > 0 then inclinenumber(I, true) end
  end
end

--- Runs lexer on the given source code.
--
-- @tparam string source The Lua source to scan.
-- @tparam ?string source_name Name of the source (optional).
-- @treturn {string,...} A list of lexed tokens.
-- @treturn {string,...} A list of semantic information (lexed strings).
-- @treturn {int,...} A list of line numbers.
function M.lex(source, source_name)
  init(source, source_name)

  while true do--outer
    local i = I
    -- inner loop allows break to be used to nicely section tests
    while true do --luacheck: ignore 512

      local p, _, r = find(z, "^([_%a][_%w]*)", i)
      if p then
        I = i + #r
        if kw[r] then
          addtoken("TK_KEYWORD", r)             -- reserved word (keyword)
        else
          addtoken("TK_NAME", r)                -- identifier
        end
        break -- (continue)
      end

      local p, _, r = find(z, "^(%.?)%d", i)
      if p then                                 -- numeral
        if r == "." then i = i + 1 end
        local _, q, r = find(z, "^%d*[%.%d]*([eE]?)", i)  --luacheck: ignore 421
        i = q + 1
        if #r == 1 then                         -- optional exponent
          if match(z, "^[%+%-]", i) then        -- optional sign
            i = i + 1
          end
        end
        local _, q = find(z, "^[_%w]*", i)
        I = q + 1
        local v = sub(z, p, q)                  -- string equivalent
        if not tonumber(v) then            -- handles hex test also
          errorline("malformed number")
        end
        addtoken("TK_NUMBER", v)
        break -- (continue)
      end

      local p, q, r, t = find(z, "^((%s)[ \t\v\f]*)", i)
      if p then
        if t == "\n" or t == "\r" then          -- newline
          inclinenumber(i, true)
        else
          I = q + 1                             -- whitespace
          addtoken("TK_SPACE", r)
        end
        break -- (continue)
      end

      local _, q = find(z, "^::", i)
      if q then
        I = q + 1
        addtoken("TK_OP", "::")
        break -- (continue)
      end

      local r = match(z, "^%p", i)
      if r then
        buff = i
        local p = find("-[\"\'.=<>~", r, 1, true)  --luacheck: ignore 421
        if p then

          -- two-level if block for punctuation/symbols
          if p <= 2 then
            if p == 1 then                      -- minus
              local c = match(z, "^%-%-(%[?)", i)
              if c then
                i = i + 2
                local sep = -1
                if c == "[" then
                  sep = skip_sep(i)
                end
                if sep >= 0 then                -- long comment
                  addtoken("TK_LCOMMENT", read_long_string(false, sep))
                else                            -- short comment
                  I = find(z, "[\n\r]", i) or (#z + 1)
                  addtoken("TK_COMMENT", sub(z, buff, I - 1))
                end
                break -- (continue)
              end
              -- (fall through for "-")
            else                                -- [ or long string
              local sep = skip_sep(i)
              if sep >= 0 then
                addtoken("TK_LSTRING", read_long_string(true, sep))
              elseif sep == -1 then
                addtoken("TK_OP", "[")
              else
                errorline("invalid long string delimiter")
              end
              break -- (continue)
            end

          elseif p <= 5 then
            if p < 5 then                       -- strings
              I = i + 1
              addtoken("TK_STRING", read_string(r))
              break -- (continue)
            end
            r = match(z, "^%.%.?%.?", i)        -- .|..|... dots
            -- (fall through)

          else                                  -- relational
            r = match(z, "^%p=?", i)
            -- (fall through)
          end
        end
        I = i + #r
        addtoken("TK_OP", r)  -- for other symbols, fall through
        break -- (continue)
      end

      local r = sub(z, i, i)
      if r ~= "" then
        I = i + 1
        addtoken("TK_OP", r)                    -- other single-char tokens
        break
      end
      addtoken("TK_EOS", "")                    -- end of stream,
      return tok, seminfo, tokln                -- exit here

    end--while inner
  end--while outer
end

return M

end);

_pack.package_cache['init'] = (function()
---------
-- LuaSrcDiet API
----
local equiv = require 'equiv'
local llex = require 'llex'
local lparser = require 'lparser'
local optlex = require 'optlex'
local optparser = require 'optparser'
local utils = require 'utils'

local concat = table.concat
local merge = utils.merge

local _  -- placeholder


local function noop ()
  return
end

local function opts_to_legacy (opts)
  local res = {}
  for key, val in pairs(opts) do
    res['opt-'..key] = val
  end
  return res
end


local M = {}

--- The module's name.
M._NAME = 'luasrcdiet'

--- The module's version number.
M._VERSION = '1.0.0'

--- The module's homepage.
M._HOMEPAGE = 'https://github.com/jirutka/luasrcdiet'

--- All optimizations disabled.
M.NONE_OPTS = {
  binequiv = false,
  comments = false,
  emptylines = false,
  entropy = false,
  eols = false,
  experimental = false,
  locals = false,
  numbers = false,
  srcequiv = false,
  strings = false,
  whitespace = false,
}

--- Basic optimizations enabled.
-- @table BASIC_OPTS
M.BASIC_OPTS = merge(M.NONE_OPTS, {
  comments = true,
  emptylines = true,
  srcequiv = true,
  whitespace = true,
})

--- Defaults.
-- @table DEFAULT_OPTS
M.DEFAULT_OPTS = merge(M.BASIC_OPTS, {
  locals = true,
  numbers = true,
})

--- Maximum optimizations enabled (all except experimental).
-- @table MAXIMUM_OPTS
M.MAXIMUM_OPTS = merge(M.DEFAULT_OPTS, {
  entropy = true,
  eols = true,
  strings = true,
})

--- Optimizes the given Lua source code.
--
-- @tparam ?{[string]=bool,...} opts Optimizations to do (default is @{DEFAULT_OPTS}).
-- @tparam string source The Lua source code to optimize.
-- @treturn string Optimized source.
-- @raise if the source is malformed, source equivalence test failed, or some
--   other error occured.
function M.optimize (opts, source)
  assert(source and type(source) == 'string',
         'bad argument #2: expected string, got a '..type(source))

  opts = opts and merge(M.NONE_OPTS, opts) or M.DEFAULT_OPTS
  local legacy_opts = opts_to_legacy(opts)

  local toklist, seminfolist, toklnlist = llex.lex(source)
  local xinfo = lparser.parse(toklist, seminfolist, toklnlist)

  optparser.print = noop
  optparser.optimize(legacy_opts, toklist, seminfolist, xinfo)

  local warn = optlex.warn  -- use this as a general warning lookup
  optlex.print = noop
  _, seminfolist = optlex.optimize(legacy_opts, toklist, seminfolist, toklnlist)
  local optim_source = concat(seminfolist)

  if opts.srcequiv and not opts.experimental then
    equiv.init(legacy_opts, llex, warn)
    equiv.source(source, optim_source)

    if warn.SRC_EQUIV then
      error('Source equivalence test failed!')
    end
  end

  return optim_source
end

return M

end);

_pack.package_cache['utils'] = (function()
---------
-- General utility functions.
--
-- **Note: This module is not part of public API!**
----
local ipairs = ipairs
local pairs = pairs

local M = {}

--- Returns a new table containing the contents of all the given tables.
-- Tables are iterated using @{pairs}, so this function is intended for tables
-- that represent *associative arrays*. Entries with duplicate keys are
-- overwritten with the values from a later table.
--
-- @tparam {table,...} ... The tables to merge.
-- @treturn table A new table.
function M.merge (...)
  local result = {}

  for _, tab in ipairs{...} do
    for key, val in pairs(tab) do
      result[key] = val
    end
  end

  return result
end

return M

end);

_pack.package_cache['lparser'] = (function()
---------
-- Lua 5.1+ parser written in Lua.
--
-- This file is part of LuaSrcDiet, based on Yueliang material.
--
-- **Notes:**
--
-- * This is a version of the native 5.1.x parser from Yueliang 0.4.0,
--   with significant modifications to handle LuaSrcDiet's needs:
--   (1) needs pre-built token tables instead of a module.method,
--   (2) lparser.error is an optional error handler (from llex),
--   (3) not full parsing, currently fakes raw/unlexed constants,
--   (4) parser() returns globalinfo, localinfo tables.
-- * NO support for 'arg' vararg functions (LUA_COMPAT_VARARG).
-- * A lot of the parser is unused, but might later be useful for
--   full-on parsing and analysis.
-- * Relaxed parsing of statement to not require "break" to be the
--   last statement of block (Lua 5.2+).
-- * Added basic support for goto and label statements, i.e. parser
--   does not crash on them (Lua 5.2+).
----
local fmt = string.format
local gmatch = string.gmatch
local pairs = pairs

local M = {}

--[[--------------------------------------------------------------------
-- variable and data structure initialization
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- initialization: main variables
----------------------------------------------------------------------

local toklist,                  -- grammar-only token tables (token table,
      seminfolist,              -- semantic information table, line number
      toklnlist,                -- table, cross-reference table)
      xreflist,
      tpos,                     -- token position

      line,                     -- start line # for error messages
      lastln,                   -- last line # for ambiguous syntax chk
      tok, seminfo, ln, xref,   -- token, semantic info, line
      nameref,                  -- proper position of <name> token
      fs,                       -- current function state
      top_fs,                   -- top-level function state

      globalinfo,               -- global variable information table
      globallookup,             -- global variable name lookup table
      localinfo,                -- local variable information table
      ilocalinfo,               -- inactive locals (prior to activation)
      ilocalrefs,               -- corresponding references to activate
      statinfo                  -- statements labeled by type

-- forward references for local functions
local explist1, expr, block, exp1, body, chunk

----------------------------------------------------------------------
-- initialization: data structures
----------------------------------------------------------------------

local block_follow = {}         -- lookahead check in chunk(), returnstat()
for v in gmatch("else elseif end until <eof>", "%S+") do
  block_follow[v] = true
end

local binopr_left = {}          -- binary operators, left priority
local binopr_right = {}         -- binary operators, right priority
for op, lt, rt in gmatch([[
{+ 6 6}{- 6 6}{* 7 7}{/ 7 7}{% 7 7}
{^ 10 9}{.. 5 4}
{~= 3 3}{== 3 3}
{< 3 3}{<= 3 3}{> 3 3}{>= 3 3}
{and 2 2}{or 1 1}
]], "{(%S+)%s(%d+)%s(%d+)}") do
  binopr_left[op] = lt + 0
  binopr_right[op] = rt + 0
end

local unopr = { ["not"] = true, ["-"] = true,
                ["#"] = true, } -- unary operators
local UNARY_PRIORITY = 8        -- priority for unary operators

--[[--------------------------------------------------------------------
-- support functions
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- formats error message and throws error (duplicated from llex)
-- * a simplified version, does not report what token was responsible
----------------------------------------------------------------------

local function errorline(s, line)
  local e = M.error or error
  e(fmt("(source):%d: %s", line or ln, s))
end

----------------------------------------------------------------------
-- handles incoming token, semantic information pairs
-- * NOTE: 'nextt' is named 'next' originally
----------------------------------------------------------------------

-- reads in next token
local function nextt()
  lastln = toklnlist[tpos]
  tok, seminfo, ln, xref
    = toklist[tpos], seminfolist[tpos], toklnlist[tpos], xreflist[tpos]
  tpos = tpos + 1
end

-- peek at next token (single lookahead for table constructor)
local function lookahead()
  return toklist[tpos]
end

----------------------------------------------------------------------
-- throws a syntax error, or if token expected is not there
----------------------------------------------------------------------

local function syntaxerror(msg)
  if tok ~= "<number>" and tok ~= "<string>" then
    if tok == "<name>" then tok = seminfo end
    tok = "'"..tok.."'"
  end
  errorline(msg.." near "..tok)
end

local function error_expected(token)
  syntaxerror("'"..token.."' expected")
end

----------------------------------------------------------------------
-- tests for a token, returns outcome
-- * return value changed to boolean
----------------------------------------------------------------------

local function testnext(c)
  if tok == c then nextt(); return true end
end

----------------------------------------------------------------------
-- check for existence of a token, throws error if not found
----------------------------------------------------------------------

local function check(c)
  if tok ~= c then error_expected(c) end
end

----------------------------------------------------------------------
-- verify existence of a token, then skip it
----------------------------------------------------------------------

local function checknext(c)
  check(c); nextt()
end

----------------------------------------------------------------------
-- throws error if condition not matched
----------------------------------------------------------------------

local function check_condition(c, msg)
  if not c then syntaxerror(msg) end
end

----------------------------------------------------------------------
-- verifies token conditions are met or else throw error
----------------------------------------------------------------------

local function check_match(what, who, where)
  if not testnext(what) then
    if where == ln then
      error_expected(what)
    else
      syntaxerror("'"..what.."' expected (to close '"..who.."' at line "..where..")")
    end
  end
end

----------------------------------------------------------------------
-- expect that token is a name, consume it and return the name
----------------------------------------------------------------------

local function str_checkname()
  check("<name>")
  local ts = seminfo
  nameref = xref
  nextt()
  return ts
end

--[[--------------------------------------------------------------------
-- variable (global|local|upvalue) handling
-- * to track locals and globals, variable management code needed
-- * entry point is singlevar() for variable lookups
-- * lookup tables (bl.locallist) are maintained awkwardly in the basic
--   block data structures, PLUS the function data structure (this is
--   an inelegant hack, since bl is nil for the top level of a function)
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- register a local variable, create local variable object, set in
-- to-activate variable list
-- * used in new_localvarliteral(), parlist(), fornum(), forlist(),
--   localfunc(), localstat()
----------------------------------------------------------------------

local function new_localvar(name, special)
  local bl = fs.bl
  local locallist
  -- locate locallist in current block object or function root object
  if bl then
    locallist = bl.locallist
  else
    locallist = fs.locallist
  end
  -- build local variable information object and set localinfo
  local id = #localinfo + 1
  localinfo[id] = {             -- new local variable object
    name = name,                -- local variable name
    xref = { nameref },         -- xref, first value is declaration
    decl = nameref,             -- location of declaration, = xref[1]
  }
  if special or name == "_ENV" then  -- "self" and "_ENV" must be not be changed
    localinfo[id].is_special = true
  end
  -- this can override a local with the same name in the same scope
  -- but first, keep it inactive until it gets activated
  local i = #ilocalinfo + 1
  ilocalinfo[i] = id
  ilocalrefs[i] = locallist
end

----------------------------------------------------------------------
-- actually activate the variables so that they are visible
-- * remember Lua semantics, e.g. RHS is evaluated first, then LHS
-- * used in parlist(), forbody(), localfunc(), localstat(), body()
----------------------------------------------------------------------

local function adjustlocalvars(nvars)
  local sz = #ilocalinfo
  -- i goes from left to right, in order of local allocation, because
  -- of something like: local a,a,a = 1,2,3 which gives a = 3
  while nvars > 0 do
    nvars = nvars - 1
    local i = sz - nvars
    local id = ilocalinfo[i]            -- local's id
    local obj = localinfo[id]
    local name = obj.name               -- name of local
    obj.act = xref                      -- set activation location
    ilocalinfo[i] = nil
    local locallist = ilocalrefs[i]     -- ref to lookup table to update
    ilocalrefs[i] = nil
    local existing = locallist[name]    -- if existing, remove old first!
    if existing then                    -- do not overlap, set special
      obj = localinfo[existing]         -- form of rem, as -id
      obj.rem = -id
    end
    locallist[name] = id                -- activate, now visible to Lua
  end
end

----------------------------------------------------------------------
-- remove (deactivate) variables in current scope (before scope exits)
-- * zap entire locallist tables since we are not allocating registers
-- * used in leaveblock(), close_func()
----------------------------------------------------------------------

local function removevars()
  local bl = fs.bl
  local locallist
  -- locate locallist in current block object or function root object
  if bl then
    locallist = bl.locallist
  else
    locallist = fs.locallist
  end
  -- enumerate the local list at current scope and deactivate 'em
  for _, id in pairs(locallist) do
    local obj = localinfo[id]
    obj.rem = xref                      -- set deactivation location
  end
end

----------------------------------------------------------------------
-- creates a new local variable given a name
-- * skips internal locals (those starting with '('), so internal
--   locals never needs a corresponding adjustlocalvars() call
-- * special is true for "self" which must not be optimized
-- * used in fornum(), forlist(), parlist(), body()
----------------------------------------------------------------------

local function new_localvarliteral(name, special)
  if name:sub(1, 1) == "(" then  -- can skip internal locals
    return
  end
  new_localvar(name, special)
end

----------------------------------------------------------------------
-- search the local variable namespace of the given fs for a match
-- * returns localinfo index
-- * used only in singlevaraux()
----------------------------------------------------------------------

local function searchvar(fs, n)
  local bl = fs.bl
  local locallist
  if bl then
    locallist = bl.locallist
    while locallist do
      if locallist[n] then return locallist[n] end  -- found
      bl = bl.prev
      locallist = bl and bl.locallist
    end
  end
  locallist = fs.locallist
  return locallist[n] or -1  -- found or not found (-1)
end

----------------------------------------------------------------------
-- handle locals, globals and upvalues and related processing
-- * search mechanism is recursive, calls itself to search parents
-- * used only in singlevar()
----------------------------------------------------------------------

local function singlevaraux(fs, n, var)
  if fs == nil then  -- no more levels?
    var.k = "VGLOBAL"  -- default is global variable
    return "VGLOBAL"
  else
    local v = searchvar(fs, n)  -- look up at current level
    if v >= 0 then
      var.k = "VLOCAL"
      var.id = v
      --  codegen may need to deal with upvalue here
      return "VLOCAL"
    else  -- not found at current level; try upper one
      if singlevaraux(fs.prev, n, var) == "VGLOBAL" then
        return "VGLOBAL"
      end
      -- else was LOCAL or UPVAL, handle here
      var.k = "VUPVAL"  -- upvalue in this level
      return "VUPVAL"
    end--if v
  end--if fs
end

----------------------------------------------------------------------
-- consume a name token, creates a variable (global|local|upvalue)
-- * used in prefixexp(), funcname()
----------------------------------------------------------------------

local function singlevar(v)
  local name = str_checkname()
  singlevaraux(fs, name, v)
  ------------------------------------------------------------------
  -- variable tracking
  ------------------------------------------------------------------
  if v.k == "VGLOBAL" then
    -- if global being accessed, keep track of it by creating an object
    local id = globallookup[name]
    if not id then
      id = #globalinfo + 1
      globalinfo[id] = {                -- new global variable object
        name = name,                    -- global variable name
        xref = { nameref },             -- xref, first value is declaration
      }
      globallookup[name] = id           -- remember it
    else
      local obj = globalinfo[id].xref
      obj[#obj + 1] = nameref           -- add xref
    end
  else
    -- local/upvalue is being accessed, keep track of it
    local obj = localinfo[v.id].xref
    obj[#obj + 1] = nameref             -- add xref
  end
end

--[[--------------------------------------------------------------------
-- state management functions with open/close pairs
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- enters a code unit, initializes elements
----------------------------------------------------------------------

local function enterblock(isbreakable)
  local bl = {}  -- per-block state
  bl.isbreakable = isbreakable
  bl.prev = fs.bl
  bl.locallist = {}
  fs.bl = bl
end

----------------------------------------------------------------------
-- leaves a code unit, close any upvalues
----------------------------------------------------------------------

local function leaveblock()
  local bl = fs.bl
  removevars()
  fs.bl = bl.prev
end

----------------------------------------------------------------------
-- opening of a function
-- * top_fs is only for anchoring the top fs, so that parser() can
--   return it to the caller function along with useful output
-- * used in parser() and body()
----------------------------------------------------------------------

local function open_func()
  local new_fs  -- per-function state
  if not fs then  -- top_fs is created early
    new_fs = top_fs
  else
    new_fs = {}
  end
  new_fs.prev = fs  -- linked list of function states
  new_fs.bl = nil
  new_fs.locallist = {}
  fs = new_fs
end

----------------------------------------------------------------------
-- closing of a function
-- * used in parser() and body()
----------------------------------------------------------------------

local function close_func()
  removevars()
  fs = fs.prev
end

--[[--------------------------------------------------------------------
-- other parsing functions
-- * for table constructor, parameter list, argument list
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- parse a function name suffix, for function call specifications
-- * used in primaryexp(), funcname()
----------------------------------------------------------------------

local function field(v)
  -- field -> ['.' | ':'] NAME
  nextt()  -- skip the dot or colon
  str_checkname()
  v.k = "VINDEXED"
end

----------------------------------------------------------------------
-- parse a table indexing suffix, for constructors, expressions
-- * used in recfield(), primaryexp()
----------------------------------------------------------------------

local function yindex()
  -- index -> '[' expr ']'
  nextt()  -- skip the '['
  expr({})
  checknext("]")
end

----------------------------------------------------------------------
-- parse a table record (hash) field
-- * used in constructor()
----------------------------------------------------------------------

local function recfield()
  -- recfield -> (NAME | '['exp1']') = exp1
  if tok == "<name>" then
    str_checkname()
  else-- tok == '['
    yindex()
  end
  checknext("=")
  expr({})
end

----------------------------------------------------------------------
-- parse a table list (array) field
-- * used in constructor()
----------------------------------------------------------------------

local function listfield(cc)
  expr(cc.v)
end

----------------------------------------------------------------------
-- parse a table constructor
-- * used in funcargs(), simpleexp()
----------------------------------------------------------------------

local function constructor(t)
  -- constructor -> '{' [ field { fieldsep field } [ fieldsep ] ] '}'
  -- field -> recfield | listfield
  -- fieldsep -> ',' | ';'
  local line = ln
  local cc = {
    v = { k = "VVOID" },
  }
  t.k = "VRELOCABLE"
  checknext("{")
  repeat
    if tok == "}" then break end
    -- closelistfield(cc) here
    local c = tok
    if c == "<name>" then  -- may be listfields or recfields
      if lookahead() ~= "=" then  -- look ahead: expression?
        listfield(cc)
      else
        recfield()
      end
    elseif c == "[" then  -- constructor_item -> recfield
      recfield()
    else  -- constructor_part -> listfield
      listfield(cc)
    end
  until not testnext(",") and not testnext(";")
  check_match("}", "{", line)
  -- lastlistfield(cc) here
end

----------------------------------------------------------------------
-- parse the arguments (parameters) of a function declaration
-- * used in body()
----------------------------------------------------------------------

local function parlist()
  -- parlist -> [ param { ',' param } ]
  local nparams = 0
  if tok ~= ")" then  -- is 'parlist' not empty?
    repeat
      local c = tok
      if c == "<name>" then  -- param -> NAME
        new_localvar(str_checkname())
        nparams = nparams + 1
      elseif c == "..." then
        nextt()
        fs.is_vararg = true
      else
        syntaxerror("<name> or '...' expected")
      end
    until fs.is_vararg or not testnext(",")
  end--if
  adjustlocalvars(nparams)
end

----------------------------------------------------------------------
-- parse the parameters of a function call
-- * contrast with parlist(), used in function declarations
-- * used in primaryexp()
----------------------------------------------------------------------

local function funcargs(f)
  local line = ln
  local c = tok
  if c == "(" then  -- funcargs -> '(' [ explist1 ] ')'
    if line ~= lastln then
      syntaxerror("ambiguous syntax (function call x new statement)")
    end
    nextt()
    if tok ~= ")" then  -- arg list is not empty?
      explist1()
    end
    check_match(")", "(", line)
  elseif c == "{" then  -- funcargs -> constructor
    constructor({})
  elseif c == "<string>" then  -- funcargs -> STRING
    nextt()  -- must use 'seminfo' before 'next'
  else
    syntaxerror("function arguments expected")
    return
  end--if c
  f.k = "VCALL"
end

--[[--------------------------------------------------------------------
-- mostly expression functions
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- parses an expression in parentheses or a single variable
-- * used in primaryexp()
----------------------------------------------------------------------

local function prefixexp(v)
  -- prefixexp -> NAME | '(' expr ')'
  local c = tok
  if c == "(" then
    local line = ln
    nextt()
    expr(v)
    check_match(")", "(", line)
  elseif c == "<name>" then
    singlevar(v)
  else
    syntaxerror("unexpected symbol")
  end--if c
end

----------------------------------------------------------------------
-- parses a prefixexp (an expression in parentheses or a single
-- variable) or a function call specification
-- * used in simpleexp(), assignment(), expr_stat()
----------------------------------------------------------------------

local function primaryexp(v)
  -- primaryexp ->
  --    prefixexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs }
  prefixexp(v)
  while true do
    local c = tok
    if c == "." then  -- field
      field(v)
    elseif c == "[" then  -- '[' exp1 ']'
      v.k = "VLOCAL"
      yindex()
    elseif c == ":" then  -- ':' NAME funcargs
      nextt()
      str_checkname()
      funcargs(v)
    elseif c == "(" or c == "<string>" or c == "{" then  -- funcargs
      funcargs(v)
    else
      return
    end--if c
  end--while
end

----------------------------------------------------------------------
-- parses general expression types, constants handled here
-- * used in subexpr()
----------------------------------------------------------------------

local function simpleexp(v)
  -- simpleexp -> NUMBER | STRING | NIL | TRUE | FALSE | ... |
  --              constructor | FUNCTION body | primaryexp
  local c = tok
  if c == "<number>" then
    v.k = "VKNUM"
  elseif c == "<string>" then
    v.k = "VK"
  elseif c == "nil" then
    v.k = "VNIL"
  elseif c == "true" then
    v.k = "VTRUE"
  elseif c == "false" then
    v.k = "VFALSE"
  elseif c == "..." then  -- vararg
    check_condition(fs.is_vararg == true,
                    "cannot use '...' outside a vararg function");
    v.k = "VVARARG"
  elseif c == "{" then  -- constructor
    constructor(v)
    return
  elseif c == "function" then
    nextt()
    body(false, ln)
    return
  else
    primaryexp(v)
    return
  end--if c
  nextt()
end

------------------------------------------------------------------------
-- Parse subexpressions. Includes handling of unary operators and binary
-- operators. A subexpr is given the rhs priority level of the operator
-- immediately left of it, if any (limit is -1 if none,) and if a binop
-- is found, limit is compared with the lhs priority level of the binop
-- in order to determine which executes first.
-- * recursively called
-- * used in expr()
------------------------------------------------------------------------

local function subexpr(v, limit)
  -- subexpr -> (simpleexp | unop subexpr) { binop subexpr }
  --   * where 'binop' is any binary operator with a priority
  --     higher than 'limit'
  local op = tok
  local uop = unopr[op]
  if uop then
    nextt()
    subexpr(v, UNARY_PRIORITY)
  else
    simpleexp(v)
  end
  -- expand while operators have priorities higher than 'limit'
  op = tok
  local binop = binopr_left[op]
  while binop and binop > limit do
    nextt()
    -- read sub-expression with higher priority
    op = subexpr({}, binopr_right[op])  -- next operator
    binop = binopr_left[op]
  end
  return op  -- return first untreated operator
end

----------------------------------------------------------------------
-- Expression parsing starts here. Function subexpr is entered with the
-- left operator (which is non-existent) priority of -1, which is lower
-- than all actual operators. Expr information is returned in parm v.
-- * used in cond(), explist1(), index(), recfield(), listfield(),
--   prefixexp(), while_stat(), exp1()
----------------------------------------------------------------------

-- this is a forward-referenced local
function expr(v)
  -- expr -> subexpr
  subexpr(v, 0)
end

--[[--------------------------------------------------------------------
-- third level parsing functions
----------------------------------------------------------------------]]

------------------------------------------------------------------------
-- parse a variable assignment sequence
-- * recursively called
-- * used in expr_stat()
------------------------------------------------------------------------

local function assignment(v)
  local c = v.v.k
  check_condition(c == "VLOCAL" or c == "VUPVAL" or c == "VGLOBAL"
                  or c == "VINDEXED", "syntax error")
  if testnext(",") then  -- assignment -> ',' primaryexp assignment
    local nv = {}  -- expdesc
    nv.v = {}
    primaryexp(nv.v)
    -- lparser.c deals with some register usage conflict here
    assignment(nv)
  else  -- assignment -> '=' explist1
    checknext("=")
    explist1()
    return  -- avoid default
  end
end

----------------------------------------------------------------------
-- parse a for loop body for both versions of the for loop
-- * used in fornum(), forlist()
----------------------------------------------------------------------

local function forbody(nvars)
  -- forbody -> DO block
  checknext("do")
  enterblock(false)  -- scope for declared variables
  adjustlocalvars(nvars)
  block()
  leaveblock()  -- end of scope for declared variables
end

----------------------------------------------------------------------
-- parse a numerical for loop, calls forbody()
-- * used in for_stat()
----------------------------------------------------------------------

local function fornum(varname)
  -- fornum -> NAME = exp1, exp1 [, exp1] DO body
  new_localvarliteral("(for index)")
  new_localvarliteral("(for limit)")
  new_localvarliteral("(for step)")
  new_localvar(varname)
  checknext("=")
  exp1()  -- initial value
  checknext(",")
  exp1()  -- limit
  if testnext(",") then
    exp1()  -- optional step
  else
    -- default step = 1
  end
  forbody(1)
end

----------------------------------------------------------------------
-- parse a generic for loop, calls forbody()
-- * used in for_stat()
----------------------------------------------------------------------

local function forlist(indexname)
  -- forlist -> NAME {, NAME} IN explist1 DO body
  -- create control variables
  new_localvarliteral("(for generator)")
  new_localvarliteral("(for state)")
  new_localvarliteral("(for control)")
  -- create declared variables
  new_localvar(indexname)
  local nvars = 1
  while testnext(",") do
    new_localvar(str_checkname())
    nvars = nvars + 1
  end
  checknext("in")
  explist1()
  forbody(nvars)
end

----------------------------------------------------------------------
-- parse a function name specification
-- * used in func_stat()
----------------------------------------------------------------------

local function funcname(v)
  -- funcname -> NAME {field} [':' NAME]
  local needself = false
  singlevar(v)
  while tok == "." do
    field(v)
  end
  if tok == ":" then
    needself = true
    field(v)
  end
  return needself
end

----------------------------------------------------------------------
-- parse the single expressions needed in numerical for loops
-- * used in fornum()
----------------------------------------------------------------------

-- this is a forward-referenced local
function exp1()
  -- exp1 -> expr
  expr({})
end

----------------------------------------------------------------------
-- parse condition in a repeat statement or an if control structure
-- * used in repeat_stat(), test_then_block()
----------------------------------------------------------------------

local function cond()
  -- cond -> expr
  expr({})  -- read condition
end

----------------------------------------------------------------------
-- parse part of an if control structure, including the condition
-- * used in if_stat()
----------------------------------------------------------------------

local function test_then_block()
  -- test_then_block -> [IF | ELSEIF] cond THEN block
  nextt()  -- skip IF or ELSEIF
  cond()
  checknext("then")
  block()  -- 'then' part
end

----------------------------------------------------------------------
-- parse a local function statement
-- * used in local_stat()
----------------------------------------------------------------------

local function localfunc()
  -- localfunc -> NAME body
  new_localvar(str_checkname())
  adjustlocalvars(1)
  body(false, ln)
end

----------------------------------------------------------------------
-- parse a local variable declaration statement
-- * used in local_stat()
----------------------------------------------------------------------

local function localstat()
  -- localstat -> NAME {',' NAME} ['=' explist1]
  local nvars = 0
  repeat
    new_localvar(str_checkname())
    nvars = nvars + 1
  until not testnext(",")
  if testnext("=") then
    explist1()
  else
    -- VVOID
  end
  adjustlocalvars(nvars)
end

----------------------------------------------------------------------
-- parse a list of comma-separated expressions
-- * used in return_stat(), localstat(), funcargs(), assignment(),
--   forlist()
----------------------------------------------------------------------

-- this is a forward-referenced local
function explist1()
  -- explist1 -> expr { ',' expr }
  local e = {}
  expr(e)
  while testnext(",") do
    expr(e)
  end
end

----------------------------------------------------------------------
-- parse function declaration body
-- * used in simpleexp(), localfunc(), func_stat()
----------------------------------------------------------------------

-- this is a forward-referenced local
function body(needself, line)
  -- body ->  '(' parlist ')' chunk END
  open_func()
  checknext("(")
  if needself then
    new_localvarliteral("self", true)
    adjustlocalvars(1)
  end
  parlist()
  checknext(")")
  chunk()
  check_match("end", "function", line)
  close_func()
end

----------------------------------------------------------------------
-- parse a code block or unit
-- * used in do_stat(), while_stat(), forbody(), test_then_block(),
--   if_stat()
----------------------------------------------------------------------

-- this is a forward-referenced local
function block()
  -- block -> chunk
  enterblock(false)
  chunk()
  leaveblock()
end

--[[--------------------------------------------------------------------
-- second level parsing functions, all with '_stat' suffix
-- * since they are called via a table lookup, they cannot be local
--   functions (a lookup table of local functions might be smaller...)
-- * stat() -> *_stat()
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- initial parsing for a for loop, calls fornum() or forlist()
-- * removed 'line' parameter (used to set debug information only)
-- * used in stat()
----------------------------------------------------------------------

local function for_stat()
  -- stat -> for_stat -> FOR (fornum | forlist) END
  local line = line
  enterblock(true)  -- scope for loop and control variables
  nextt()  -- skip 'for'
  local varname = str_checkname()  -- first variable name
  local c = tok
  if c == "=" then
    fornum(varname)
  elseif c == "," or c == "in" then
    forlist(varname)
  else
    syntaxerror("'=' or 'in' expected")
  end
  check_match("end", "for", line)
  leaveblock()  -- loop scope (`break' jumps to this point)
end

----------------------------------------------------------------------
-- parse a while-do control structure, body processed by block()
-- * used in stat()
----------------------------------------------------------------------

local function while_stat()
  -- stat -> while_stat -> WHILE cond DO block END
  local line = line
  nextt()  -- skip WHILE
  cond()  -- parse condition
  enterblock(true)
  checknext("do")
  block()
  check_match("end", "while", line)
  leaveblock()
end

----------------------------------------------------------------------
-- parse a repeat-until control structure, body parsed by chunk()
-- * originally, repeatstat() calls breakstat() too if there is an
--   upvalue in the scope block; nothing is actually lexed, it is
--   actually the common code in breakstat() for closing of upvalues
-- * used in stat()
----------------------------------------------------------------------

local function repeat_stat()
  -- stat -> repeat_stat -> REPEAT block UNTIL cond
  local line = line
  enterblock(true)  -- loop block
  enterblock(false)  -- scope block
  nextt()  -- skip REPEAT
  chunk()
  check_match("until", "repeat", line)
  cond()
  -- close upvalues at scope level below
  leaveblock()  -- finish scope
  leaveblock()  -- finish loop
end

----------------------------------------------------------------------
-- parse an if control structure
-- * used in stat()
----------------------------------------------------------------------

local function if_stat()
  -- stat -> if_stat -> IF cond THEN block
  --                    {ELSEIF cond THEN block} [ELSE block] END
  local line = line
  test_then_block()  -- IF cond THEN block
  while tok == "elseif" do
    test_then_block()  -- ELSEIF cond THEN block
  end
  if tok == "else" then
    nextt()  -- skip ELSE
    block()  -- 'else' part
  end
  check_match("end", "if", line)
end

----------------------------------------------------------------------
-- parse a return statement
-- * used in stat()
----------------------------------------------------------------------

local function return_stat()
  -- stat -> return_stat -> RETURN explist
  nextt()  -- skip RETURN
  local c = tok
  if block_follow[c] or c == ";" then
    -- return no values
  else
    explist1()  -- optional return values
  end
end

----------------------------------------------------------------------
-- parse a break statement
-- * used in stat()
----------------------------------------------------------------------

local function break_stat()
  -- stat -> break_stat -> BREAK
  local bl = fs.bl
  nextt()  -- skip BREAK
  while bl and not bl.isbreakable do -- find a breakable block
    bl = bl.prev
  end
  if not bl then
    syntaxerror("no loop to break")
  end
end

----------------------------------------------------------------------
-- parse a label statement
-- * this function has been added later, it just parses label statement
--   without any validation!
-- * used in stat()
----------------------------------------------------------------------

local function label_stat()
  -- stat -> label_stat -> '::' NAME '::'
  nextt()  -- skip '::'
  str_checkname()
  checknext("::")
end

----------------------------------------------------------------------
-- parse a goto statement
-- * this function has been added later, it just parses goto statement
--   without any validation!
-- * used in stat()
----------------------------------------------------------------------

local function goto_stat()
  -- stat -> goto_stat -> GOTO NAME
  nextt()  -- skip GOTO
  str_checkname()
end

----------------------------------------------------------------------
-- parse a function call with no returns or an assignment statement
-- * the struct with .prev is used for name searching in lparse.c,
--   so it is retained for now; present in assignment() also
-- * used in stat()
----------------------------------------------------------------------

local function expr_stat()
  local id = tpos - 1
  -- stat -> expr_stat -> func | assignment
  local v = { v = {} }
  primaryexp(v.v)
  if v.v.k == "VCALL" then  -- stat -> func
    -- call statement uses no results
    statinfo[id] = "call"
  else  -- stat -> assignment
    v.prev = nil
    assignment(v)
    statinfo[id] = "assign"
  end
end

----------------------------------------------------------------------
-- parse a function statement
-- * used in stat()
----------------------------------------------------------------------

local function function_stat()
  -- stat -> function_stat -> FUNCTION funcname body
  local line = line
  nextt()  -- skip FUNCTION
  local needself = funcname({})
  body(needself, line)
end

----------------------------------------------------------------------
-- parse a simple block enclosed by a DO..END pair
-- * used in stat()
----------------------------------------------------------------------

local function do_stat()
  -- stat -> do_stat -> DO block END
  local line = line
  nextt()  -- skip DO
  block()
  check_match("end", "do", line)
end

----------------------------------------------------------------------
-- parse a statement starting with LOCAL
-- * used in stat()
----------------------------------------------------------------------

local function local_stat()
  -- stat -> local_stat -> LOCAL FUNCTION localfunc
  --                    -> LOCAL localstat
  nextt()  -- skip LOCAL
  if testnext("function") then  -- local function?
    localfunc()
  else
    localstat()
  end
end

--[[--------------------------------------------------------------------
-- main functions, top level parsing functions
-- * accessible functions are: init(lexer), parser()
-- * [entry] -> parser() -> chunk() -> stat()
----------------------------------------------------------------------]]

----------------------------------------------------------------------
-- initial parsing for statements, calls '_stat' suffixed functions
-- * used in chunk()
----------------------------------------------------------------------

local stat_call = {             -- lookup for calls in stat()
  ["if"] = if_stat,
  ["while"] = while_stat,
  ["do"] = do_stat,
  ["for"] = for_stat,
  ["repeat"] = repeat_stat,
  ["function"] = function_stat,
  ["local"] = local_stat,
  ["return"] = return_stat,
  ["break"] = break_stat,
  ["goto"] = goto_stat,
  ["::"] = label_stat,
}

local function stat()
  -- stat -> if_stat while_stat do_stat for_stat repeat_stat
  --         function_stat local_stat return_stat break_stat
  --         expr_stat
  line = ln  -- may be needed for error messages
  local c = tok
  local fn = stat_call[c]
  -- handles: if while do for repeat function local return break
  if fn then
    statinfo[tpos - 1] = c
    fn()
    -- return must be last statement
    if c == "return" then return true end
  else
    expr_stat()
  end
  return false
end

----------------------------------------------------------------------
-- parse a chunk, which consists of a bunch of statements
-- * used in parser(), body(), block(), repeat_stat()
----------------------------------------------------------------------

-- this is a forward-referenced local
function chunk()
  -- chunk -> { stat [';'] }
  local islast = false
  while not islast and not block_follow[tok] do
    islast = stat()
    testnext(";")
  end
end

----------------------------------------------------------------------
-- initialization function
----------------------------------------------------------------------

local function init(tokorig, seminfoorig, toklnorig)
  tpos = 1                      -- token position
  top_fs = {}                   -- reset top level function state
  ------------------------------------------------------------------
  -- set up grammar-only token tables; impedance-matching...
  -- note that constants returned by the lexer is source-level, so
  -- for now, fake(!) constant tokens (TK_NUMBER|TK_STRING|TK_LSTRING)
  ------------------------------------------------------------------
  local j = 1
  toklist, seminfolist, toklnlist, xreflist = {}, {}, {}, {}
  for i = 1, #tokorig do
    local tok = tokorig[i]
    local yep = true
    if tok == "TK_KEYWORD" or tok == "TK_OP" then
      tok = seminfoorig[i]
    elseif tok == "TK_NAME" then
      tok = "<name>"
      seminfolist[j] = seminfoorig[i]
    elseif tok == "TK_NUMBER" then
      tok = "<number>"
      seminfolist[j] = 0  -- fake!
    elseif tok == "TK_STRING" or tok == "TK_LSTRING" then
      tok = "<string>"
      seminfolist[j] = ""  -- fake!
    elseif tok == "TK_EOS" then
      tok = "<eof>"
    else
      -- non-grammar tokens; ignore them
      yep = false
    end
    if yep then  -- set rest of the information
      toklist[j] = tok
      toklnlist[j] = toklnorig[i]
      xreflist[j] = i
      j = j + 1
    end
  end--for
  ------------------------------------------------------------------
  -- initialize data structures for variable tracking
  ------------------------------------------------------------------
  globalinfo, globallookup, localinfo = {}, {}, {}
  ilocalinfo, ilocalrefs = {}, {}
  statinfo = {}  -- experimental
end

----------------------------------------------------------------------
-- performs parsing, returns parsed data structure
----------------------------------------------------------------------

function M.parse(tokens, seminfo, tokens_ln)
  init(tokens, seminfo, tokens_ln)

  open_func()
  fs.is_vararg = true  -- main func. is always vararg
  nextt()  -- read first token
  chunk()
  check("<eof>")
  close_func()
  return {  -- return everything
    globalinfo = globalinfo,
    localinfo = localinfo,
    statinfo = statinfo,
    toklist = toklist,
    seminfolist = seminfolist,
    toklnlist = toklnlist,
    xreflist = xreflist,
  }
end

return M

end);

_pack.package_cache['fs'] = (function()
---------
-- Utility functions for operations on a file system.
--
-- **Note: This module is not part of public API!**
----
local fmt = string.format
local open = io.open

local UTF8_BOM = '\239\187\191'

local function normalize_io_error (name, err)
  if err:sub(1, #name + 2) == name..': ' then
    err = err:sub(#name + 3)
  end
  return err
end

local M = {}

--- Reads the specified file and returns its content as string.
--
-- @tparam string filename Path of the file to read.
-- @tparam string mode The mode in which to open the file, see @{io.open} (default: "r").
-- @treturn[1] string A content of the file.
-- @treturn[2] nil
-- @treturn[2] string An error message.
function M.read_file (filename, mode)
  local handler, err = open(filename, mode or 'r')
  if not handler then
    return nil, fmt('Could not open %s for reading: %s',
                    filename, normalize_io_error(filename, err))
  end

  local content, err = handler:read('*a')  --luacheck: ignore 411
  if not content then
    return nil, fmt('Could not read %s: %s', filename, normalize_io_error(filename, err))
  end

  handler:close()

  if content:sub(1, #UTF8_BOM) == UTF8_BOM then
    content = content:sub(#UTF8_BOM + 1)
  end

  return content
end

--- Writes the given data to the specified file.
--
-- @tparam string filename Path of the file to write.
-- @tparam string data The data to write.
-- @tparam ?string mode The mode in which to open the file, see @{io.open} (default: "w").
-- @treturn[1] true
-- @treturn[2] nil
-- @treturn[2] string An error message.
function M.write_file (filename, data, mode)
  local handler, err = open(filename, mode or 'w')
  if not handler then
    return nil, fmt('Could not open %s for writing: %s',
                    filename, normalize_io_error(filename, err))
  end

  local _, err = handler:write(data)  --luacheck: ignore 411
  if err then
    return nil, fmt('Could not write %s: %s', filename, normalize_io_error(filename, err))
  end

  handler:flush()
  handler:close()

  return true
end

return M

end);

_pack.package_cache['optlex'] = (function()
---------
-- This module does lexer-based optimizations.
--
-- **Notes:**
--
-- * TODO: General string delimiter conversion optimizer.
-- * TODO: (numbers) warn if overly significant digit.
----
local char = string.char
local find = string.find
local match = string.match
local rep = string.rep
local sub = string.sub
local tonumber = tonumber
local tostring = tostring

local print                     -- set in optimize()

local M = {}

-- error function, can override by setting own function into module
M.error = error

M.warn = {}                       -- table for warning flags

local stoks, sinfos, stoklns    -- source lists

local is_realtoken = {          -- significant (grammar) tokens
  TK_KEYWORD = true,
  TK_NAME = true,
  TK_NUMBER = true,
  TK_STRING = true,
  TK_LSTRING = true,
  TK_OP = true,
  TK_EOS = true,
}
local is_faketoken = {          -- whitespace (non-grammar) tokens
  TK_COMMENT = true,
  TK_LCOMMENT = true,
  TK_EOL = true,
  TK_SPACE = true,
}

local opt_details               -- for extra information

--- Returns true if current token is at the start of a line.
--
-- It skips over deleted tokens via recursion.
--
-- @tparam int i
-- @treturn bool
local function atlinestart(i)
  local tok = stoks[i - 1]
  if i <= 1 or tok == "TK_EOL" then
    return true
  elseif tok == "" then
    return atlinestart(i - 1)
  end
  return false
end

--- Returns true if current token is at the end of a line.
--
-- It skips over deleted tokens via recursion.
--
-- @tparam int i
-- @treturn bool
local function atlineend(i)
  local tok = stoks[i + 1]
  if i >= #stoks or tok == "TK_EOL" or tok == "TK_EOS" then
    return true
  elseif tok == "" then
    return atlineend(i + 1)
  end
  return false
end

--- Counts comment EOLs inside a long comment.
--
-- In order to keep line numbering, EOLs need to be reinserted.
--
-- @tparam string lcomment
-- @treturn int
local function commenteols(lcomment)
  local sep = #match(lcomment, "^%-%-%[=*%[")
  local z = sub(lcomment, sep + 1, -(sep - 1))  -- remove delims
  local i, c = 1, 0
  while true do
    local p, _, r, s = find(z, "([\r\n])([\r\n]?)", i)
    if not p then break end     -- if no matches, done
    i = p + 1
    c = c + 1
    if #s > 0 and r ~= s then   -- skip CRLF or LFCR
      i = i + 1
    end
  end
  return c
end

--- Compares two tokens (i, j) and returns the whitespace required.
--
-- See documentation for a reference table of interactions.
--
-- Only two grammar/real tokens are being considered:
--
-- * if `""`, no separation is needed,
-- * if `" "`, then at least one whitespace (or EOL) is required.
--
-- Note: This doesn't work at the start or the end or for EOS!
--
-- @tparam int i
-- @tparam int j
-- @treturn string
local function checkpair(i, j)
  local t1, t2 = stoks[i], stoks[j]

  if t1 == "TK_STRING" or t1 == "TK_LSTRING" or
     t2 == "TK_STRING" or t2 == "TK_LSTRING" then
    return ""

  elseif t1 == "TK_OP" or t2 == "TK_OP" then
    if (t1 == "TK_OP" and (t2 == "TK_KEYWORD" or t2 == "TK_NAME")) or
       (t2 == "TK_OP" and (t1 == "TK_KEYWORD" or t1 == "TK_NAME")) then
      return ""
    end
    if t1 == "TK_OP" and t2 == "TK_OP" then
      -- for TK_OP/TK_OP pairs, see notes in technotes.txt
      local op, op2 = sinfos[i], sinfos[j]
      if (match(op, "^%.%.?$") and match(op2, "^%.")) or
         (match(op, "^[~=<>]$") and op2 == "=") or
         (op == "[" and (op2 == "[" or op2 == "=")) then
        return " "
      end
      return ""
    end
    -- "TK_OP" + "TK_NUMBER" case
    local op = sinfos[i]
    if t2 == "TK_OP" then op = sinfos[j] end
    if match(op, "^%.%.?%.?$") then
      return " "
    end
    return ""

  else-- "TK_KEYWORD" | "TK_NAME" | "TK_NUMBER" then
    return " "

  end
end

--- Repack tokens, removing deletions caused by optimization process.
local function repack_tokens()
  local dtoks, dinfos, dtoklns = {}, {}, {}
  local j = 1
  for i = 1, #stoks do
    local tok = stoks[i]
    if tok ~= "" then
      dtoks[j], dinfos[j], dtoklns[j] = tok, sinfos[i], stoklns[i]
      j = j + 1
    end
  end
  stoks, sinfos, stoklns = dtoks, dinfos, dtoklns
end

--- Does number optimization.
--
-- Optimization using string formatting functions is one way of doing this,
-- but here, we consider all cases and handle them separately (possibly an
-- idiotic approach...).
--
-- Scientific notation being generated is not in canonical form, this may or
-- may not be a bad thing.
--
-- Note: Intermediate portions need to fit into a normal number range.
--
-- Optimizations can be divided based on number patterns:
--
-- * hexadecimal:
--   (1) no need to remove leading zeros, just skip to (2)
--   (2) convert to integer if size equal or smaller
--       * change if equal size -> lose the 'x' to reduce entropy
--   (3) number is then processed as an integer
--   (4) note: does not make 0[xX] consistent
-- * integer:
--   (1) reduce useless fractional part, if present, e.g. 123.000 -> 123.
--   (2) remove leading zeros, e.g. 000123
-- * float:
--   (1) split into digits dot digits
--   (2) if no integer portion, take as zero (can omit later)
--   (3) handle degenerate .000 case, after which the fractional part
--       must be non-zero (if zero, it's matched as float .0)
--   (4) remove trailing zeros for fractional portion
--   (5) p.q where p > 0 and q > 0 cannot be shortened any more
--   (6) otherwise p == 0 and the form is .q, e.g. .000123
--   (7) if scientific shorter, convert, e.g. .000123 -> 123e-6
-- * scientific:
--   (1) split into (digits dot digits) [eE] ([+-] digits)
--   (2) if significand is zero, just use .0
--   (3) remove leading zeros for significand
--   (4) shift out trailing zeros for significand
--   (5) examine exponent and determine which format is best:
--       number with fraction, or scientific
--
-- Note: Number with fraction and scientific number is never converted
-- to integer, because Lua 5.3 distinguishes between integers and floats.
--
--
-- @tparam int i
local function do_number(i)
  local before = sinfos[i]      -- 'before'
  local z = before              -- working representation
  local y                       -- 'after', if better
  --------------------------------------------------------------------
  if match(z, "^0[xX]") then            -- hexadecimal number
    local v = tostring(tonumber(z))
    if #v <= #z then
      z = v  -- change to integer, AND continue
    else
      return  -- no change; stick to hex
    end
  end

  if match(z, "^%d+$") then        -- integer
    if tonumber(z) > 0 then
      y = match(z, "^0*([1-9]%d*)$")  -- remove leading zeros
    else
      y = "0"  -- basic zero
    end

  elseif not match(z, "[eE]") then      -- float
    local p, q = match(z, "^(%d*)%.(%d*)$")  -- split
    if p == "" then p = "0" end  -- int part zero
    if q == "" then q = "0" end  -- fraction part zero
    if tonumber(q) == 0 and p == "0" then
      y = ".0"  -- degenerate .000 to .0
    else
      -- now, q > 0 holds and p is a number
      local zeros_cnt = #match(q, "0*$")  -- remove trailing zeros
      if zeros_cnt > 0 then
        q = sub(q, 1, #q - zeros_cnt)
      end
      -- if p > 0, nothing else we can do to simplify p.q case
      if tonumber(p) > 0 then
        y = p.."."..q
      else
        y = "."..q  -- tentative, e.g. .000123
        local v = #match(q, "^0*")  -- # leading spaces
        local w = #q - v            -- # significant digits
        local nv = tostring(#q)
        -- e.g. compare 123e-6 versus .000123
        if w + 2 + #nv < 1 + #q then
          y = sub(q, -w).."e-"..nv
        end
      end
    end

  else                                  -- scientific number
    local sig, ex = match(z, "^([^eE]+)[eE]([%+%-]?%d+)$")
    ex = tonumber(ex)
    -- if got ".", shift out fractional portion of significand
    local p, q = match(sig, "^(%d*)%.(%d*)$")
    if p then
      ex = ex - #q
      sig = p..q
    end
    if tonumber(sig) == 0 then
      y = ".0"  -- basic float zero
    else
      local v = #match(sig, "^0*")  -- remove leading zeros
      sig = sub(sig, v + 1)
      v = #match(sig, "0*$") -- shift out trailing zeros
      if v > 0 then
        sig = sub(sig, 1, #sig - v)
        ex = ex + v
      end
      -- examine exponent and determine which format is best
      local nex = tostring(ex)
      if ex >= 0 and (ex <= 1 + #nex) then  -- a float
        y = sig..rep("0", ex).."."
      elseif ex < 0 and (ex >= -#sig) then  -- fraction, e.g. .123
        v = #sig + ex
        y = sub(sig, 1, v).."."..sub(sig, v + 1)
      elseif ex < 0 and (#nex >= -ex - #sig) then
        -- e.g. compare 1234e-5 versus .01234
        -- gives: #sig + 1 + #nex >= 1 + (-ex - #sig) + #sig
        --     -> #nex >= -ex - #sig
        v = -ex - #sig
        y = "."..rep("0", v)..sig
      else  -- non-canonical scientific representation
        y = sig.."e"..ex
      end
    end--if sig
  end

  if y and y ~= sinfos[i] then
    if opt_details then
      print("<number> (line "..stoklns[i]..") "..sinfos[i].." -> "..y)
      opt_details = opt_details + 1
    end
    sinfos[i] = y
  end
end

--- Does string optimization.
--
-- Note: It works on well-formed strings only!
--
-- Optimizations on characters can be summarized as follows:
--
--     \a\b\f\n\r\t\v -- no change
--     \\             -- no change
--     \"\'           -- depends on delim, other can remove \
--     \[\]           -- remove \
--     \<char>        -- general escape, remove \  (Lua 5.1 only)
--     \<eol>         -- normalize the EOL only
--     \ddd           -- if \a\b\f\n\r\t\v, change to latter
--                       if other < ascii 32, keep ddd but zap leading zeros
--                                            but cannot have following digits
--                       if >= ascii 32, translate it into the literal, then also
--                                       do escapes for \\,\",\' cases
--     <other>        -- no change
--
-- Switch delimiters if string becomes shorter.
--
-- @tparam int I
local function do_string(I)
  local info = sinfos[I]
  local delim = sub(info, 1, 1)                 -- delimiter used
  local ndelim = (delim == "'") and '"' or "'"  -- opposite " <-> '
  local z = sub(info, 2, -2)                    -- actual string
  local i = 1
  local c_delim, c_ndelim = 0, 0                -- "/' counts

  while i <= #z do
    local c = sub(z, i, i)

    if c == "\\" then                   -- escaped stuff
      local j = i + 1
      local d = sub(z, j, j)
      local p = find("abfnrtv\\\n\r\"\'0123456789", d, 1, true)

      if not p then                     -- \<char> -- remove \  (Lua 5.1 only)
        z = sub(z, 1, i - 1)..sub(z, j)
        i = i + 1

      elseif p <= 8 then                -- \a\b\f\n\r\t\v\\
        i = i + 2                       -- no change

      elseif p <= 10 then               -- \<eol> -- normalize EOL
        local eol = sub(z, j, j + 1)
        if eol == "\r\n" or eol == "\n\r" then
          z = sub(z, 1, i).."\n"..sub(z, j + 2)
        elseif p == 10 then  -- \r case
          z = sub(z, 1, i).."\n"..sub(z, j + 1)
        end
        i = i + 2

      elseif p <= 12 then               -- \"\' -- remove \ for ndelim
        if d == delim then
          c_delim = c_delim + 1
          i = i + 2
        else
          c_ndelim = c_ndelim + 1
          z = sub(z, 1, i - 1)..sub(z, j)
          i = i + 1
        end

      else                              -- \ddd -- various steps
        local s = match(z, "^(%d%d?%d?)", j)
        j = i + 1 + #s                  -- skip to location
        local cv = tonumber(s)
        local cc = char(cv)
        p = find("\a\b\f\n\r\t\v", cc, 1, true)
        if p then                       -- special escapes
          s = "\\"..sub("abfnrtv", p, p)
        elseif cv < 32 then             -- normalized \ddd
          if match(sub(z, j, j), "%d") then
            -- if a digit follows, \ddd cannot be shortened
            s = "\\"..s
          else
            s = "\\"..cv
          end
        elseif cc == delim then         -- \<delim>
          s = "\\"..cc
          c_delim = c_delim + 1
        elseif cc == "\\" then          -- \\
          s = "\\\\"
        else                            -- literal character
          s = cc
          if cc == ndelim then
            c_ndelim = c_ndelim + 1
          end
        end
        z = sub(z, 1, i - 1)..s..sub(z, j)
        i = i + #s

      end--if p

    else-- c ~= "\\"                    -- <other> -- no change
      i = i + 1
      if c == ndelim then  -- count ndelim, for switching delimiters
        c_ndelim = c_ndelim + 1
      end

    end--if c
  end--while

  -- Switching delimiters, a long-winded derivation:
  -- (1) delim takes 2+2*c_delim bytes, ndelim takes c_ndelim bytes
  -- (2) delim becomes c_delim bytes, ndelim becomes 2+2*c_ndelim bytes
  -- simplifying the condition (1)>(2) --> c_delim > c_ndelim
  if c_delim > c_ndelim then
    i = 1
    while i <= #z do
      local p, _, r = find(z, "([\'\"])", i)
      if not p then break end
      if r == delim then                -- \<delim> -> <delim>
        z = sub(z, 1, p - 2)..sub(z, p)
        i = p
      else-- r == ndelim                -- <ndelim> -> \<ndelim>
        z = sub(z, 1, p - 1).."\\"..sub(z, p)
        i = p + 2
      end
    end--while
    delim = ndelim  -- actually change delimiters
  end

  z = delim..z..delim
  if z ~= sinfos[I] then
    if opt_details then
      print("<string> (line "..stoklns[I]..") "..sinfos[I].." -> "..z)
      opt_details = opt_details + 1
    end
    sinfos[I] = z
  end
end

--- Does long string optimization.
--
-- * remove first optional newline
-- * normalize embedded newlines
-- * reduce '=' separators in delimiters if possible
--
-- Note: warning flagged if trailing whitespace found, not trimmed.
--
-- @tparam int I
local function do_lstring(I)
  local info = sinfos[I]
  local delim1 = match(info, "^%[=*%[")  -- cut out delimiters
  local sep = #delim1
  local delim2 = sub(info, -sep, -1)
  local z = sub(info, sep + 1, -(sep + 1))  -- lstring without delims
  local y = ""
  local i = 1

  while true do
    local p, _, r, s = find(z, "([\r\n])([\r\n]?)", i)
    -- deal with a single line
    local ln
    if not p then
      ln = sub(z, i)
    elseif p >= i then
      ln = sub(z, i, p - 1)
    end
    if ln ~= "" then
      -- flag a warning if there are trailing spaces, won't optimize!
      if match(ln, "%s+$") then
        M.warn.LSTRING = "trailing whitespace in long string near line "..stoklns[I]
      end
      y = y..ln
    end
    if not p then  -- done if no more EOLs
      break
    end
    -- deal with line endings, normalize them
    i = p + 1
    if p then
      if #s > 0 and r ~= s then  -- skip CRLF or LFCR
        i = i + 1
      end
      -- skip first newline, which can be safely deleted
      if not(i == 1 and i == p) then
        y = y.."\n"
      end
    end
  end--while

  -- handle possible deletion of one or more '=' separators
  if sep >= 3 then
    local chk, okay = sep - 1
    -- loop to test ending delimiter with less of '=' down to zero
    while chk >= 2 do
      local delim = "%]"..rep("=", chk - 2).."%]"
      if not match(y.."]", delim) then okay = chk end
      chk = chk - 1
    end
    if okay then  -- change delimiters
      sep = rep("=", okay - 2)
      delim1, delim2 = "["..sep.."[", "]"..sep.."]"
    end
  end

  sinfos[I] = delim1..y..delim2
end

--- Does long comment optimization.
--
-- * trim trailing whitespace
-- * normalize embedded newlines
-- * reduce '=' separators in delimiters if possible
--
-- Note: It does not remove first optional newline.
--
-- @tparam int I
local function do_lcomment(I)
  local info = sinfos[I]
  local delim1 = match(info, "^%-%-%[=*%[")  -- cut out delimiters
  local sep = #delim1
  local delim2 = sub(info, -(sep - 2), -1)
  local z = sub(info, sep + 1, -(sep - 1))  -- comment without delims
  local y = ""
  local i = 1

  while true do
    local p, _, r, s = find(z, "([\r\n])([\r\n]?)", i)
    -- deal with a single line, extract and check trailing whitespace
    local ln
    if not p then
      ln = sub(z, i)
    elseif p >= i then
      ln = sub(z, i, p - 1)
    end
    if ln ~= "" then
      -- trim trailing whitespace if non-empty line
      local ws = match(ln, "%s*$")
      if #ws > 0 then ln = sub(ln, 1, -(ws + 1)) end
      y = y..ln
    end
    if not p then  -- done if no more EOLs
      break
    end
    -- deal with line endings, normalize them
    i = p + 1
    if p then
      if #s > 0 and r ~= s then  -- skip CRLF or LFCR
        i = i + 1
      end
      y = y.."\n"
    end
  end--while

  -- handle possible deletion of one or more '=' separators
  sep = sep - 2
  if sep >= 3 then
    local chk, okay = sep - 1
    -- loop to test ending delimiter with less of '=' down to zero
    while chk >= 2 do
      local delim = "%]"..rep("=", chk - 2).."%]"
      if not match(y, delim) then okay = chk end
      chk = chk - 1
    end
    if okay then  -- change delimiters
      sep = rep("=", okay - 2)
      delim1, delim2 = "--["..sep.."[", "]"..sep.."]"
    end
  end

  sinfos[I] = delim1..y..delim2
end

--- Does short comment optimization.
--
-- * trim trailing whitespace
--
-- @tparam int i
local function do_comment(i)
  local info = sinfos[i]
  local ws = match(info, "%s*$")        -- just look from end of string
  if #ws > 0 then
    info = sub(info, 1, -(ws + 1))      -- trim trailing whitespace
  end
  sinfos[i] = info
end

--- Returns true if string found in long comment.
--
-- This is a feature to keep copyright or license texts.
--
-- @tparam bool opt_keep
-- @tparam string info
-- @treturn bool
local function keep_lcomment(opt_keep, info)
  if not opt_keep then return false end  -- option not set
  local delim1 = match(info, "^%-%-%[=*%[")  -- cut out delimiters
  local sep = #delim1
  local z = sub(info, sep + 1, -(sep - 1))  -- comment without delims
  if find(z, opt_keep, 1, true) then  -- try to match
    return true
  end
end

--- The main entry point.
--
-- * currently, lexer processing has 2 passes
-- * processing is done on a line-oriented basis, which is easier to
--   grok due to the next point...
-- * since there are various options that can be enabled or disabled,
--   processing is a little messy or convoluted
--
-- @tparam {[string]=bool,...} option
-- @tparam {string,...} toklist
-- @tparam {string,...} semlist
-- @tparam {int,...} toklnlist
-- @treturn {string,...} toklist
-- @treturn {string,...} semlist
-- @treturn {int,...} toklnlist
function M.optimize(option, toklist, semlist, toklnlist)
  -- Set option flags.
  local opt_comments = option["opt-comments"]
  local opt_whitespace = option["opt-whitespace"]
  local opt_emptylines = option["opt-emptylines"]
  local opt_eols = option["opt-eols"]
  local opt_strings = option["opt-strings"]
  local opt_numbers = option["opt-numbers"]
  local opt_x = option["opt-experimental"]
  local opt_keep = option.KEEP
  opt_details = option.DETAILS and 0  -- upvalues for details display
  print = M.print or _G.print
  if opt_eols then  -- forced settings, otherwise won't work properly
    opt_comments = true
    opt_whitespace = true
    opt_emptylines = true
  elseif opt_x then
    opt_whitespace = true
  end

  -- Variable initialization.
  stoks, sinfos, stoklns                -- set source lists
    = toklist, semlist, toklnlist
  local i = 1                           -- token position
  local tok, info                       -- current token
  local prev    -- position of last grammar token
                -- on same line (for TK_SPACE stuff)

  -- Changes a token, info pair.
  local function settoken(tok, info, I)  --luacheck: ignore 431
    I = I or i
    stoks[I] = tok or ""
    sinfos[I] = info or ""
  end

  -- Experimental optimization for ';' operator.
  if opt_x then
    while true do
      tok, info = stoks[i], sinfos[i]
      if tok == "TK_EOS" then           -- end of stream/pass
        break
      elseif tok == "TK_OP" and info == ";" then
        -- ';' operator found, since it is entirely optional, set it
        -- as a space to let whitespace optimization do the rest
        settoken("TK_SPACE", " ")
      end
      i = i + 1
    end
    repack_tokens()
  end

  -- Processing loop (PASS 1)
  i = 1
  while true do
    tok, info = stoks[i], sinfos[i]

    local atstart = atlinestart(i)      -- set line begin flag
    if atstart then prev = nil end

    if tok == "TK_EOS" then             -- end of stream/pass
      break

    elseif tok == "TK_KEYWORD" or       -- keywords, identifiers,
           tok == "TK_NAME" or          -- operators
           tok == "TK_OP" then
      -- TK_KEYWORD and TK_OP can't be optimized without a big
      -- optimization framework; it would be more of an optimizing
      -- compiler, not a source code compressor
      -- TK_NAME that are locals needs parser to analyze/optimize
      prev = i

    elseif tok == "TK_NUMBER" then      -- numbers
      if opt_numbers then
        do_number(i)  -- optimize
      end
      prev = i

    elseif tok == "TK_STRING" or        -- strings, long strings
           tok == "TK_LSTRING" then
      if opt_strings then
        if tok == "TK_STRING" then
          do_string(i)  -- optimize
        else
          do_lstring(i)  -- optimize
        end
      end
      prev = i

    elseif tok == "TK_COMMENT" then     -- short comments
      if opt_comments then
        if i == 1 and sub(info, 1, 1) == "#" then
          -- keep shbang comment, trim whitespace
          do_comment(i)
        else
          -- safe to delete, as a TK_EOL (or TK_EOS) always follows
          settoken()  -- remove entirely
        end
      elseif opt_whitespace then        -- trim whitespace only
        do_comment(i)
      end

    elseif tok == "TK_LCOMMENT" then    -- long comments
      if keep_lcomment(opt_keep, info) then
        -- if --keep, we keep a long comment if <msg> is found;
        -- this is a feature to keep copyright or license texts
        if opt_whitespace then          -- trim whitespace only
          do_lcomment(i)
        end
        prev = i
      elseif opt_comments then
        local eols = commenteols(info)

        -- prepare opt_emptylines case first, if a disposable token
        -- follows, current one is safe to dump, else keep a space;
        -- it is implied that the operation is safe for '-', because
        -- current is a TK_LCOMMENT, and must be separate from a '-'
        if is_faketoken[stoks[i + 1]] then
          settoken()  -- remove entirely
          tok = ""
        else
          settoken("TK_SPACE", " ")
        end

        -- if there are embedded EOLs to keep and opt_emptylines is
        -- disabled, then switch the token into one or more EOLs
        if not opt_emptylines and eols > 0 then
          settoken("TK_EOL", rep("\n", eols))
        end

        -- if optimizing whitespaces, force reinterpretation of the
        -- token to give a chance for the space to be optimized away
        if opt_whitespace and tok ~= "" then
          i = i - 1  -- to reinterpret
        end
      else                              -- disabled case
        if opt_whitespace then          -- trim whitespace only
          do_lcomment(i)
        end
        prev = i
      end

    elseif tok == "TK_EOL" then         -- line endings
      if atstart and opt_emptylines then
        settoken()  -- remove entirely
      elseif info == "\r\n" or info == "\n\r" then
        -- normalize the rest of the EOLs for CRLF/LFCR only
        -- (note that TK_LCOMMENT can change into several EOLs)
        settoken("TK_EOL", "\n")
      end

    elseif tok == "TK_SPACE" then       -- whitespace
      if opt_whitespace then
        if atstart or atlineend(i) then
          -- delete leading and trailing whitespace
          settoken()  -- remove entirely
        else

          -- at this point, since leading whitespace have been removed,
          -- there should be a either a real token or a TK_LCOMMENT
          -- prior to hitting this whitespace; the TK_LCOMMENT case
          -- only happens if opt_comments is disabled; so prev ~= nil
          local ptok = stoks[prev]
          if ptok == "TK_LCOMMENT" then
            -- previous TK_LCOMMENT can abut with anything
            settoken()  -- remove entirely
          else
            -- prev must be a grammar token; consecutive TK_SPACE
            -- tokens is impossible when optimizing whitespace
            local ntok = stoks[i + 1]
            if is_faketoken[ntok] then
              -- handle special case where a '-' cannot abut with
              -- either a short comment or a long comment
              if (ntok == "TK_COMMENT" or ntok == "TK_LCOMMENT") and
                 ptok == "TK_OP" and sinfos[prev] == "-" then
                -- keep token
              else
                settoken()  -- remove entirely
              end
            else--is_realtoken
              -- check a pair of grammar tokens, if can abut, then
              -- delete space token entirely, otherwise keep one space
              local s = checkpair(prev, i + 1)
              if s == "" then
                settoken()  -- remove entirely
              else
                settoken("TK_SPACE", " ")
              end
            end
          end

        end
      end

    else
      error("unidentified token encountered")
    end

    i = i + 1
  end--while
  repack_tokens()

  -- Processing loop (PASS 2)
  if opt_eols then
    i = 1
    -- Aggressive EOL removal only works with most non-grammar tokens
    -- optimized away because it is a rather simple scheme -- basically
    -- it just checks 'real' token pairs around EOLs.
    if stoks[1] == "TK_COMMENT" then
      -- first comment still existing must be shbang, skip whole line
      i = 3
    end
    while true do
      tok = stoks[i]

      if tok == "TK_EOS" then           -- end of stream/pass
        break

      elseif tok == "TK_EOL" then       -- consider each TK_EOL
        local t1, t2 = stoks[i - 1], stoks[i + 1]
        if is_realtoken[t1] and is_realtoken[t2] then  -- sanity check
          local s = checkpair(i - 1, i + 1)
          if s == "" or t2 == "TK_EOS" then
            settoken()  -- remove entirely
          end
        end
      end--if tok

      i = i + 1
    end--while
    repack_tokens()
  end

  if opt_details and opt_details > 0 then print() end -- spacing
  return stoks, sinfos, stoklns
end

return M

end);


local LuaSrcDiet = require('init')

local Options = {
    comments = true,
    whitespace = true,
    emptylines = true,
    eols = true,
    strings = false,
    numbers = true,
    locals = true,
    entropy = true,
    srcequiv = false,
    binequiv = false,
    experimental = true,
}

return function(source)
    print("(luasrcdiet) minifying script...")
    return LuaSrcDiet.optimize(Options, source)
end
