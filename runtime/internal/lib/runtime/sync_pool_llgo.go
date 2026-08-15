package runtime

// Keep the dynamic TLS bridge linked without adding it to the standard sync
// package's source dependency graph.
import _ "github.com/goplus/llgo/runtime/internal/clite/tls"
