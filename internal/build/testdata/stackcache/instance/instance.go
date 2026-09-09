package instance

import "github.com/xgo-dev/llgo/internal/build/testdata/stackcache/generic"

func Value() int { return generic.Launch(42) }
