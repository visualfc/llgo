package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/py"
	"github.com/goplus/lib/py/numpy"
	"github.com/goplus/lib/py/std"
	"github.com/goplus/lib/py/torch"
)

func main() {
	a := py.List(
		py.List(1.0, 2.0, 3.0),
		py.List(4.0, 5.0, 6.0),
		py.List(7.0, 8.0, 9.0),
	)
	b := py.List(
		py.List(9.0, 8.0, 7.0),
		py.List(6.0, 5.0, 4.0),
		py.List(3.0, 2.0, 1.0),
	)
	x := numpy.Add(a, b)
	rows := x.GetAttrString(c.Str("tolist")).CallObject(nil)
	if rows == nil || rows.ListLen() != 3 {
		panic("unexpected numpy.add row count")
	}
	for rowIndex := 0; rowIndex < 3; rowIndex++ {
		row := rows.ListItem(rowIndex)
		if row == nil || row.ListLen() != 3 {
			panic("unexpected numpy.add column count")
		}
		for columnIndex := 0; columnIndex < 3; columnIndex++ {
			if got := row.ListItem(columnIndex).Float64(); got != 10 {
				panic("unexpected numpy.add value")
			}
		}
	}
	tensor := torch.Tensor(py.List(
		py.List(1.0, 2.0),
		py.List(3.0, 4.0),
	))
	tensorRows := tensor.GetAttrString(c.Str("tolist")).CallObject(nil)
	if tensorRows == nil || tensorRows.ListLen() != 2 {
		panic("unexpected torch.Tensor row count")
	}
	for rowIndex := 0; rowIndex < 2; rowIndex++ {
		row := tensorRows.ListItem(rowIndex)
		if row == nil || row.ListLen() != 2 {
			panic("unexpected torch.Tensor column count")
		}
		for columnIndex := 0; columnIndex < 2; columnIndex++ {
			if got, want := row.ListItem(columnIndex).Float64(), float64(rowIndex*2+columnIndex+1); got != want {
				panic("unexpected torch.Tensor value")
			}
		}
	}
	std.Print(py.Str("a+b ="), x)
}
