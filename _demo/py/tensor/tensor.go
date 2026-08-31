package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/py"
	"github.com/goplus/lib/py/torch"
)

func main() {
	tensor := torch.Tensor(py.List(
		py.List(1.0, 2.0),
		py.List(3.0, 4.0),
	))
	rows := tensor.GetAttrString(c.Str("tolist")).CallObject(nil)
	if rows == nil || rows.ListLen() != 2 {
		panic("unexpected torch.Tensor row count")
	}
	for rowIndex := 0; rowIndex < 2; rowIndex++ {
		row := rows.ListItem(rowIndex)
		if row == nil || row.ListLen() != 2 {
			panic("unexpected torch.Tensor column count")
		}
		for columnIndex := 0; columnIndex < 2; columnIndex++ {
			if got, want := row.ListItem(columnIndex).Float64(), float64(rowIndex*2+columnIndex+1); got != want {
				panic("unexpected torch.Tensor value")
			}
		}
	}
}
