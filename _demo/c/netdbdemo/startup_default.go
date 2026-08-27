//go:build !windows

package main

func startupNetwork() func() { return func() {} }
