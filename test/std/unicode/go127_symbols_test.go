//go:build go1.27

package unicode_test

import (
	"testing"
	"unicode"
)

func TestGo127RangeTables(t *testing.T) {
	tables := map[string]*unicode.RangeTable{
		"Beria_Erfe":              unicode.Beria_Erfe,
		"Garay":                   unicode.Garay,
		"Gurung_Khema":            unicode.Gurung_Khema,
		"IDS_Unary_Operator":      unicode.IDS_Unary_Operator,
		"ID_Compat_Math_Continue": unicode.ID_Compat_Math_Continue,
		"ID_Compat_Math_Start":    unicode.ID_Compat_Math_Start,
		"Kirat_Rai":               unicode.Kirat_Rai,
		"Modifier_Combining_Mark": unicode.Modifier_Combining_Mark,
		"Ol_Onal":                 unicode.Ol_Onal,
		"Sidetic":                 unicode.Sidetic,
		"Sunuwar":                 unicode.Sunuwar,
		"Tai_Yo":                  unicode.Tai_Yo,
		"Todhri":                  unicode.Todhri,
		"Tolong_Siki":             unicode.Tolong_Siki,
		"Tulu_Tigalari":           unicode.Tulu_Tigalari,
	}
	for name, table := range tables {
		if table == nil {
			t.Fatalf("%s is nil", name)
		}
		var first rune
		switch {
		case len(table.R16) != 0:
			first = rune(table.R16[0].Lo)
		case len(table.R32) != 0:
			first = rune(table.R32[0].Lo)
		default:
			t.Fatalf("%s is empty", name)
		}
		if !unicode.Is(table, first) {
			t.Fatalf("%s does not contain its first range value %U", name, first)
		}
	}
}
