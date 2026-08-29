//go:build go1.27

package types_test

import (
	"go/token"
	"go/types"
	"hash/maphash"
	"testing"
)

func TestHashers(t *testing.T) {
	field := types.NewField(token.NoPos, nil, "Value", types.Typ[types.Int], false)
	left := types.NewStruct([]*types.Var{field}, []string{"left"})
	right := types.NewStruct([]*types.Var{field}, []string{"right"})

	regular := types.Hasher{}
	if regular.Equal(left, right) {
		t.Fatal("Hasher ignored struct tags")
	}
	var leftHash maphash.Hash
	leftHash.SetSeed(maphash.MakeSeed())
	regular.Hash(&leftHash, left)
	var repeatedLeftHash maphash.Hash
	repeatedLeftHash.SetSeed(leftHash.Seed())
	regular.Hash(&repeatedLeftHash, left)
	if leftHash.Sum64() != repeatedLeftHash.Sum64() {
		t.Fatal("Hasher produced different hashes for the same type and seed")
	}

	withoutTags := types.HasherIgnoreTags{}
	if !withoutTags.Equal(left, right) {
		t.Fatal("HasherIgnoreTags compared struct tags")
	}
	var rightHash maphash.Hash
	rightHash.SetSeed(leftHash.Seed())
	withoutTags.Hash(&rightHash, right)
	var leftWithoutTagsHash maphash.Hash
	leftWithoutTagsHash.SetSeed(leftHash.Seed())
	withoutTags.Hash(&leftWithoutTagsHash, left)
	if leftWithoutTagsHash.Sum64() != rightHash.Sum64() {
		t.Fatal("HasherIgnoreTags produced different hashes for equal types")
	}
}

func TestInstanceAndTypeListStrings(t *testing.T) {
	parameter := types.NewTypeParam(
		types.NewTypeName(token.NoPos, nil, "T", nil),
		types.Universe.Lookup("any").Type(),
	)
	named := types.NewNamed(
		types.NewTypeName(token.NoPos, nil, "Box", nil),
		types.NewStruct(nil, nil),
		nil,
	)
	named.SetTypeParams([]*types.TypeParam{parameter})
	instantiated, err := types.Instantiate(nil, named, []types.Type{types.Typ[types.Int]}, true)
	if err != nil {
		t.Fatal(err)
	}
	arguments := instantiated.(*types.Named).TypeArgs()
	if got := arguments.String(); got != "[int]" {
		t.Fatalf("TypeList.String = %q", got)
	}
	if got := named.TypeParams().String(); got != "[T]" {
		t.Fatalf("TypeParamList.String = %q", got)
	}
	instance := types.Instance{TypeArgs: arguments, Type: instantiated}
	if got := instance.String(); got != "[int]Box[int]" {
		t.Fatalf("Instance.String = %q", got)
	}
}
