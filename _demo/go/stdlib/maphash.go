package main

import "hash/maphash"

func testMapHash() {
	seed := maphash.MakeSeed()
	_ = maphash.MakeSeed()

	var first, second maphash.Hash
	first.SetSeed(seed)
	if n, err := first.WriteString("hello"); err != nil || n != 5 {
		panic("maphash WriteString")
	}
	hello := first.Sum64()
	first.Reset()
	if n, err := first.WriteString("hello"); err != nil || n != 5 || first.Sum64() != hello {
		panic("maphash Reset")
	}

	second.SetSeed(seed)
	if n, err := second.Write([]byte("test")); err != nil || n != 4 {
		panic("maphash Write")
	}
	second.Reset()
	if err := second.WriteByte('A'); err != nil {
		panic("maphash WriteByte")
	}
	_ = second.Sum64()

	data := []byte("test data")
	if maphash.Bytes(seed, data) != maphash.String(seed, string(data)) {
		panic("maphash Bytes/String")
	}
}
