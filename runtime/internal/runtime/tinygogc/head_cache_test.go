package tinygogc

import "testing"

func TestMarkHeadCache(t *testing.T) {
	var c markHeadCache
	check := func(block, head uintptr, hit bool) {
		t.Helper()
		got, ok := c.lookup(block)
		if ok != hit || (ok && got != head) {
			t.Fatalf("lookup(%d)=(%d,%v), want (%d,%v)", block, got, ok, head, hit)
		}
	}
	check(2000, 0, false) // Tail miss must fall back to the actual metadata.
	c.remember(1000, 2000)
	check(999, 0, false)
	check(1000, 1000, true)
	check(1999, 1000, true)
	check(2000, 0, false)
	c.remember(1000, 3000)
	c.remember(1000, 2500) // Never shrink a previously proven interval.
	check(2999, 1000, true)
	c.remember(3000, 4000)
	check(3000, 3000, true)
	check(3999, 3000, true)
	check(4000, 0, false)
	for _, r := range c.ranges[:2] {
		if r.head < 4000 || r.end < 4000 {
			t.Fatal("stored unencoded heap block indexes")
		}
	}
	c.remember(8, 8)
	c.remember(9, 8)
	c.remember(8, 9)
	c.remember(1, ^uintptr(0))
	check(8, 0, false)
	c.remember(4000, 5000)
	c.remember(5000, 6000)
	c.remember(6000, 7000)
	check(1000, 0, false) // Round-robin eviction, not an unbounded index.
	check(3500, 3000, true)
	check(6500, 6000, true)
	c.reset()
	check(3500, 0, false)
	if c != (markHeadCache{}) {
		t.Fatal("collection reset retained prior indexes")
	}
	c.remember(0, markHeadMinBlocks-1)
	check(0, 0, false)
	c.remember(0, markHeadMinBlocks)
	check(0, 0, true)
	check(markHeadMinBlocks-1, 0, true)
	check(markHeadMinBlocks, 0, false)
}
