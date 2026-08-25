//go:build !nogc && !baremetal && !windows

package runtime

func enableForeignThreadRegistration() {}
