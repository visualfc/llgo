package cl

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	llssa "github.com/xgo-dev/llgo/ssa"
	"golang.org/x/tools/go/ssa"
)

// ReceiverNilChecks preserves implicit address-taking in pointer method
// selections. SSA folds (*p).M into p.M and represents promoted value fields
// with FieldAddr, neither of which retains the required nil dereference.
// The maps are immutable after collection and shared by package lowering.
type ReceiverNilChecks struct {
	calls  map[token.Pos]receiverNilCheck
	values map[token.Pos]receiverNilCheck
}

type receiverNilCheck struct {
	pos     token.Pos
	address bool
}

// CollectReceiverNilChecks uses checked selections, not selector spelling:
// (*T).M is a method expression, whereas (*p).M selects a method value.
// infos may include both an original package and its runtime source overlay.
func CollectReceiverNilChecks(files []*ast.File, infos ...*types.Info) *ReceiverNilChecks {
	selections := make(map[*ast.SelectorExpr]receiverNilCheck)
	for _, info := range infos {
		if info != nil {
			for expr, sel := range info.Selections {
				if isPointerMethodSelection(sel) {
					selections[expr] = receiverNilCheck{expr.Pos(), receiverNeedsAddressCheck(sel)}
				}
			}
		}
	}
	if len(selections) == 0 {
		return nil
	}
	checks := &ReceiverNilChecks{calls: make(map[token.Pos]receiverNilCheck), values: make(map[token.Pos]receiverNilCheck)}
	for expr, check := range selections {
		checks.values[expr.Sel.Pos()] = check
	}
	for _, file := range files {
		ast.Inspect(file, func(node ast.Node) bool {
			call, ok := node.(*ast.CallExpr)
			if !ok {
				return true
			}
			selector, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
			if check, found := selections[selector]; ok && found {
				checks.calls[call.Lparen] = check
			}
			return true
		})
	}
	return checks
}

func isPointerMethodSelection(sel *types.Selection) bool {
	if sel == nil || sel.Kind() != types.MethodVal {
		return false
	}
	sig, ok := sel.Obj().Type().(*types.Signature)
	return ok && sig.Recv() != nil && isPointerGoType(sig.Recv().Type())
}

func receiverNeedsAddressCheck(sel *types.Selection) bool {
	if !isPointerMethodSelection(sel) {
		return false
	}
	// An embedded pointer is already a pointer receiver and may legally be
	// nil. A value field, in contrast, requires the address of the field;
	// selecting it through a nil enclosing pointer must panic.
	typ := sel.Recv()
	indices := sel.Index()
	for _, index := range indices[:len(indices)-1] {
		if ptr, ok := types.Unalias(typ).Underlying().(*types.Pointer); ok {
			typ = ptr.Elem()
		}
		st, ok := types.Unalias(typ).Underlying().(*types.Struct)
		if !ok || index >= st.NumFields() {
			return false
		}
		typ = st.Field(index).Type()
	}
	return !isPointerGoType(typ)
}

func (p *context) checkMethodCallReceiver(b llssa.Builder, call *ssa.CallCommon) {
	if len(call.Args) == 0 {
		return
	}
	// Promoted method expressions and interface adapters also contain
	// implicit field-address arithmetic, but have no source selector or call
	// position. Keep the required base checks in the generated wrapper itself
	// so storing (*Outer).M in a function value cannot bypass them. The final
	// declared pointer receiver may still legally be nil.
	if isPointerMethodWrapperCall(p.goFn, call) {
		p.checkAddressedMethodReceiver(b, call.Args[0], receiverNilCheck{pos: p.goFn.Pos()})
	}
	checks := p.options.ReceiverNilChecks
	if checks == nil {
		return
	}
	if check, ok := checks.calls[call.Pos()]; ok {
		// SSA has evaluated the argument expressions by this instruction.
		// Checking while compiling FieldAddr would move the panic before
		// those side effects. The saved receiver also avoids re-reading a
		// variable that an argument expression may have reassigned.
		p.checkAddressedMethodReceiver(b, call.Args[0], check)
	}
}

func isPointerMethodWrapperCall(owner *ssa.Function, call *ssa.CallCommon) bool {
	if owner == nil || !(strings.HasPrefix(owner.Synthetic, "wrapper for ") || strings.HasPrefix(owner.Synthetic, "thunk for ")) {
		return false
	}
	fn := call.StaticCallee()
	return fn != nil && fn.Signature.Recv() != nil && isPointerGoType(fn.Signature.Recv().Type())
}

// collectReceiverNilDerefChecks protects pre-existing pointer loads in the
// receiver expression before LLVM can assume their bases are non-nil. Merely
// checking at the later method call would leave an earlier null load as UB.
// Address-only selections have no load and keep their check at call/creation.
func collectReceiverNilDerefChecks(fn *ssa.Function, checks *ReceiverNilChecks) map[*ssa.UnOp]token.Pos {
	var loads map[*ssa.UnOp]token.Pos
	var mark func(ssa.Value, token.Pos)
	mark = func(value ssa.Value, pos token.Pos) {
		switch value := value.(type) {
		case *ssa.UnOp:
			if value.Op != token.MUL {
				return
			}
			if !isKnownNonNilAddr(value.X) && !isWrapNilCheckCall(value.X) {
				if loads == nil {
					loads = make(map[*ssa.UnOp]token.Pos)
				}
				loads[value] = pos
			}
			mark(value.X, pos)
		case *ssa.FieldAddr:
			mark(value.X, pos)
		}
	}
	for _, block := range fn.Blocks {
		for _, instr := range block.Instrs {
			switch instr := instr.(type) {
			case ssa.CallInstruction:
				call := instr.Common()
				if len(call.Args) == 0 {
					continue
				}
				if isPointerMethodWrapperCall(fn, call) {
					mark(call.Args[0], fn.Pos())
				} else if checks != nil {
					if check, ok := checks.calls[call.Pos()]; ok {
						mark(call.Args[0], check.pos)
					}
				}
			case *ssa.MakeClosure:
				if checks != nil && len(instr.Bindings) != 0 {
					if check, ok := checks.values[instr.Pos()]; ok {
						mark(instr.Bindings[0], check.pos)
					}
				}
			}
		}
	}
	return loads
}

func (p *context) checkBoundMethodReceiver(b llssa.Builder, closure *ssa.MakeClosure) {
	checks := p.options.ReceiverNilChecks
	if checks == nil || len(closure.Bindings) == 0 {
		return
	}
	if check, ok := checks.values[closure.Pos()]; ok {
		// Method-value formation must panic now, even if the value is never
		// invoked; there is no later argument-evaluation phase to preserve.
		p.checkAddressedMethodReceiver(b, closure.Bindings[0], check)
	}
}

func (p *context) checkAddressedMethodReceiver(b llssa.Builder, receiver ssa.Value, check receiverNilCheck) {
	if isKnownNonNilAddr(receiver) || isWrapNilCheckCall(receiver) {
		return
	}
	if load, ok := receiver.(*ssa.UnOp); ok && !check.address {
		if _, protected := p.receiverNilDerefChecks[load]; protected {
			return
		}
	}
	if !check.address && !methodReceiverHasUncheckedBase(receiver) {
		return
	}
	p.recordPanicSite(b, check.pos)
	p.emitNilDerefBaseCheck(b, receiver)
	if check.address {
		b.AssertNilDeref(p.compileValue(b, receiver))
	}
}

func methodReceiverHasUncheckedBase(receiver ssa.Value) bool {
	switch receiver := receiver.(type) {
	case *ssa.UnOp:
		return receiver.Op == token.MUL && !isKnownNonNilAddr(receiver.X) && !isWrapNilCheckCall(receiver.X)
	case *ssa.FieldAddr:
		return !isKnownNonNilAddr(receiver.X) && !isWrapNilCheckCall(receiver.X)
	}
	return false
}
