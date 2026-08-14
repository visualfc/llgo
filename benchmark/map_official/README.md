# Official-style map benchmarks

This package ports the core Go runtime map benchmarks that are supported by
LLGo. The same test source can be run with the standard Go runtime, LLGo's
default map implementation, and LLGo's Swiss map implementation.

Use identical benchmark flags for comparable results:

```bash
go test ./benchmark/map_official \\
  -run '^$' -bench '^BenchmarkMap(AccessHit|AccessMiss|AssignExists|Delete)$' \\
  -benchtime=100ms -count=3

llgo test ./benchmark/map_official \\
  -run '^$' -bench '^BenchmarkMap(AccessHit|AccessMiss|AssignExists|Delete)$' \\
  -benchtime=100ms -count=3

llgo test -tags swissmap ./benchmark/map_official \\
  -run '^$' -bench '^BenchmarkMap(AccessHit|AccessMiss|AssignExists|Delete)$' \\
  -benchtime=100ms -count=3
```

By default the benchmark runs the upstream-selected map sizes (`len=6`,
`len=64`, and `len=65536`). Add `-mapbench=true` after the package arguments
to enable the complete size matrix.
