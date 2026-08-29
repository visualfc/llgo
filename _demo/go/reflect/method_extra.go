package main

import "reflect"

type methodBase struct{ Offset int }

func (b methodBase) Add(v int) int { return b.Offset + v }

type methodValue struct {
	methodBase
	Scale int
}

type embeddedPoint struct {
	X int
	Y int
}

func (p *embeddedPoint) Set(x, y int) { p.X, p.Y = x, y }

type embeddedPointOnly struct{ *embeddedPoint }
type embeddedPointWithField struct {
	*embeddedPoint
	N int
}
type embeddedPointValue struct{ embeddedPoint }

func callPromotedSet(target any) {
	value := reflect.ValueOf(target)
	if value.Kind() == reflect.Pointer {
		indirect := value.Elem()
		if indirect.MethodByName("Set").IsValid() {
			value = indirect
		}
	}
	method := value.MethodByName("Set")
	if !method.IsValid() {
		panic("promoted pointer method")
	}
	method.Call([]reflect.Value{reflect.ValueOf(100), reflect.ValueOf(200)})
}

func (v methodValue) Sum(values ...int) int {
	total := v.Scale
	for _, value := range values {
		total += value
	}
	return total
}

func (v *methodValue) Mixed(i32 int32, i64 int64, f64 float64) float64 {
	return float64(i32) + float64(i64) + f64 + float64(v.Scale)
}

func testMethodExtras() {
	v := methodValue{methodBase: methodBase{Offset: 10}, Scale: 2}
	typ := reflect.TypeOf(v)
	promoted, ok := typ.MethodByName("Add")
	if !ok || promoted.Func.Call([]reflect.Value{reflect.ValueOf(v), reflect.ValueOf(5)})[0].Int() != 15 {
		panic("promoted method")
	}
	bound := reflect.ValueOf(v).MethodByName("Add")
	if bound.Call([]reflect.Value{reflect.ValueOf(7)})[0].Int() != 17 {
		panic("bound value method")
	}
	variadic := reflect.ValueOf(v).MethodByName("Sum")
	if variadic.Call([]reflect.Value{reflect.ValueOf(3), reflect.ValueOf(4)})[0].Int() != 9 {
		panic("variadic Call")
	}
	if variadic.CallSlice([]reflect.Value{reflect.ValueOf([]int{3, 4, 5})})[0].Int() != 14 {
		panic("variadic CallSlice")
	}
	mixed := reflect.ValueOf(&v).MethodByName("Mixed")
	result := mixed.Call([]reflect.Value{reflect.ValueOf(int32(3)), reflect.ValueOf(int64(4)), reflect.ValueOf(5.5)})
	if result[0].Float() != 14.5 {
		panic("pointer receiver mixed ABI")
	}

	var iface interface{ Add(int) int } = v
	ifaceValue := reflect.ValueOf(&iface).Elem()
	if ifaceValue.Method(0).Call([]reflect.Value{reflect.ValueOf(8)})[0].Int() != 18 {
		panic("interface method")
	}

	pointOnlyValue := embeddedPointOnly{embeddedPoint: &embeddedPoint{X: 10, Y: 20}}
	callPromotedSet(pointOnlyValue)
	if pointOnlyValue.X != 100 || pointOnlyValue.Y != 200 {
		panic("promoted pointer mutation through value")
	}
	pointOnlyPointer := embeddedPointOnly{embeddedPoint: &embeddedPoint{X: 10, Y: 20}}
	callPromotedSet(&pointOnlyPointer)
	if pointOnlyPointer.X != 100 || pointOnlyPointer.Y != 200 {
		panic("promoted pointer mutation through pointer")
	}
	pointWithFieldValue := embeddedPointWithField{embeddedPoint: &embeddedPoint{X: 10, Y: 20}}
	callPromotedSet(pointWithFieldValue)
	if pointWithFieldValue.X != 100 || pointWithFieldValue.Y != 200 {
		panic("promoted pointer mutation with sibling field")
	}
	pointWithFieldPointer := embeddedPointWithField{embeddedPoint: &embeddedPoint{X: 10, Y: 20}}
	callPromotedSet(&pointWithFieldPointer)
	if pointWithFieldPointer.X != 100 || pointWithFieldPointer.Y != 200 {
		panic("promoted pointer mutation through aggregate pointer")
	}
	pointValue := &embeddedPointValue{embeddedPoint: embeddedPoint{X: 10, Y: 20}}
	callPromotedSet(pointValue)
	if pointValue.X != 100 || pointValue.Y != 200 {
		panic("promoted addressable value mutation")
	}
}
