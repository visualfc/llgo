package mapfast

import (
	"strconv"
	"testing"
)

const mapSize = 1024

var (
	intSink  int
	boolSink bool
)

type structKey struct {
	value uint64
}

func makeInt32Map() map[int32]int {
	m := make(map[int32]int, mapSize)
	for i := 0; i < mapSize; i++ {
		m[int32(i)] = i
	}
	return m
}

func makeUint64Map() map[uint64]int {
	m := make(map[uint64]int, mapSize)
	for i := 0; i < mapSize; i++ {
		m[uint64(i)] = i
	}
	return m
}

func makeStringKeys(long bool) []string {
	keys := make([]string, mapSize)
	for i := range keys {
		prefix := "k"
		if long {
			prefix = "map-fast-path-long-string-key-"
		}
		keys[i] = prefix + strconv.Itoa(i)
	}
	return keys
}

func makeStringMap(keys []string) map[string]int {
	m := make(map[string]int, len(keys))
	for i, key := range keys {
		m[key] = i
	}
	return m
}

func makeStructMap() map[structKey]int {
	m := make(map[structKey]int, mapSize)
	for i := 0; i < mapSize; i++ {
		m[structKey{uint64(i)}] = i
	}
	return m
}

func BenchmarkMapReadHit(b *testing.B) {
	b.Run("Int32", func(b *testing.B) {
		m := makeInt32Map()
		value := 0
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			value += m[int32(i&(mapSize-1))]
		}
		intSink = value
	})
	b.Run("Uint64", func(b *testing.B) {
		m := makeUint64Map()
		value := 0
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			value += m[uint64(i&(mapSize-1))]
		}
		intSink = value
	})
	for _, long := range []bool{false, true} {
		name := "StringShort"
		if long {
			name = "StringLong"
		}
		b.Run(name, func(b *testing.B) {
			keys := makeStringKeys(long)
			m := makeStringMap(keys)
			value := 0
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				value += m[keys[i&(mapSize-1)]]
			}
			intSink = value
		})
	}
	b.Run("StructControl", func(b *testing.B) {
		m := makeStructMap()
		value := 0
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			value += m[structKey{uint64(i & (mapSize - 1))}]
		}
		intSink = value
	})
}

func BenchmarkMapReadMiss(b *testing.B) {
	b.Run("Int32", func(b *testing.B) {
		m := makeInt32Map()
		ok := false
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_, ok = m[int32(mapSize+(i&(mapSize-1)))]
		}
		boolSink = ok
	})
	b.Run("Uint64", func(b *testing.B) {
		m := makeUint64Map()
		ok := false
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_, ok = m[uint64(mapSize+(i&(mapSize-1)))]
		}
		boolSink = ok
	})
	for _, long := range []bool{false, true} {
		name := "StringShort"
		prefix := "missing-"
		if long {
			name = "StringLong"
			prefix = "map-fast-path-missing-long-key-"
		}
		b.Run(name, func(b *testing.B) {
			keys := makeStringKeys(long)
			missing := make([]string, mapSize)
			for i := range missing {
				missing[i] = prefix + strconv.Itoa(i)
			}
			m := makeStringMap(keys)
			ok := false
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, ok = m[missing[i&(mapSize-1)]]
			}
			boolSink = ok
		})
	}
	b.Run("StructControl", func(b *testing.B) {
		m := makeStructMap()
		ok := false
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_, ok = m[structKey{uint64(mapSize + (i & (mapSize - 1)))}]
		}
		boolSink = ok
	})
}

func BenchmarkMapAssignExisting(b *testing.B) {
	b.Run("Int32", func(b *testing.B) {
		m := makeInt32Map()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			m[int32(i&(mapSize-1))] = i
		}
	})
	b.Run("Uint64", func(b *testing.B) {
		m := makeUint64Map()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			m[uint64(i&(mapSize-1))] = i
		}
	})
	for _, long := range []bool{false, true} {
		name := "StringShort"
		if long {
			name = "StringLong"
		}
		b.Run(name, func(b *testing.B) {
			keys := makeStringKeys(long)
			m := makeStringMap(keys)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				m[keys[i&(mapSize-1)]] = i
			}
		})
	}
	b.Run("StructControl", func(b *testing.B) {
		m := makeStructMap()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			m[structKey{uint64(i & (mapSize - 1))}] = i
		}
	})
}

func BenchmarkMapDeleteInsert(b *testing.B) {
	b.Run("Int32", func(b *testing.B) {
		m := makeInt32Map()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			key := int32(i & (mapSize - 1))
			delete(m, key)
			m[key] = i
		}
	})
	b.Run("Uint64", func(b *testing.B) {
		m := makeUint64Map()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			key := uint64(i & (mapSize - 1))
			delete(m, key)
			m[key] = i
		}
	})
	for _, long := range []bool{false, true} {
		name := "StringShort"
		if long {
			name = "StringLong"
		}
		b.Run(name, func(b *testing.B) {
			keys := makeStringKeys(long)
			m := makeStringMap(keys)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				key := keys[i&(mapSize-1)]
				delete(m, key)
				m[key] = i
			}
		})
	}
	b.Run("StructControl", func(b *testing.B) {
		m := makeStructMap()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			key := structKey{uint64(i & (mapSize - 1))}
			delete(m, key)
			m[key] = i
		}
	})
}
