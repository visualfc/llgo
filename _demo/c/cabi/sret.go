package main

type sretArray struct{ values [9]float32 }

func sretStep(value sretArray) sretArray {
	value.values[0]++
	return value
}

func sretLoop(value sretArray) sretArray {
	for i := 0; i < 128*1024; i++ {
		value = sretStep(value)
	}
	return value
}

func testSRet() {
	value := sretArray{values: [9]float32{1, 2, 3, 4, 5, 6, 7, 8, 9}}
	for i := 0; i < 128*1024; i++ {
		value = sretStep(value)
	}
	value = sretLoop(value)
	if value.values[0] != 2*128*1024+1 || value.values[8] != 9 {
		panic("large aggregate sret")
	}
	var buffer []byte
	for i := 0; i < 128*1024; i++ {
		buffer = append(buffer, byte(i))
	}
	if len(buffer) != 128*1024 || buffer[0] != 0 || buffer[len(buffer)-1] != 255 {
		panic("sret regression allocation path")
	}
}
