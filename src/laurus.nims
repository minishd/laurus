--nimcache:nimcache

--mm:refc
--opt:speed

if defined(release):
  switch("passL", "-s") # Strip debug symbols
  switch("passC", "-flto") # Link time optimisations
  --define:danger
  --debuginfo:off