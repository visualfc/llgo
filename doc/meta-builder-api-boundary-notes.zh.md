# internal/meta.Builder 对外接口边界的讨论笔记

> 背景：在 [PR #1989](https://github.com/xgo-dev/llgo/pull/1989)（`internal/meta` mmap 化）落地过程中，
> 讨论 `ssa`/`cl`/`internal/build`/`internal/deadcode` 之间如何调用 `meta.Builder`/`meta.GlobalSummary`
> 时形成的结论。本文档记录当前已落地的 Builder API 边界，以及仍可后续优化的独立话题。

## 结论（已达成一致）

1. **`ssa.Package` 直接持有 `*meta.Builder` 是合理的**，不需要为它抽一个接口。
   理由：`aPackage` 本来就直接持有 `llvm.Module` 这类"构建产物"类型；`*meta.Builder` 和
   `llvm.Module` 性质一样——都是编译过程中逐步攒起来、最终产出一个制品（`.ll` / `meta.PackageMeta`）
   的可变构建器。`ssa` 目前也只有这一个使用方，为了单一实现引入接口层没有实际收益，
   反而会把 `LocalSymbol` 这类 ID 类型在两边各定义一份、平添一层无谓的转换。
2. **不引入 `Def*`/`Use*` 这种全局命名 taxonomy**。`AddTypeChild`/`AddMethodSlot`/`AddIfaceMethod`/
   `MarkReflect` 这些"定义类"事实的方法名本身已经足够清楚，不强行统一前缀。
3. **写侧的 dispatch tag（`addEdge` 的 `kind` 参数）已经收敛为私有细节，但读侧的 `Kind` 语义不受影响**。
   这里必须把两层 kind 分清楚，讨论过程中容易混：
   - **读侧 / 查询侧**：`FuncDemand.Kind` 字段以及 `DemandUseIface`/`DemandIfaceMethod`/
     `DemandNamedMethod`/`DemandReflectMethod` 这四个常量。因为 `GlobalSummary` 是"按 symbol
     取出一整条混合列表，再按种类分桶"的查询模式（`UseIface(sym)`/`UseIfaceMethod(sym)`/
     `UseNamedMethod(sym)`/`HasReflectMethod(sym)`，见 `internal/meta/global.go`
     296-349 行），所以每条 `FuncDemand` 记录必须自带"我是哪种"的标记。**这四个 `Demand*`
     常量和 `Kind` 字段继续导出，不做任何改动**——这是 `internal/meta`/`internal/deadcode`
     之间真实需要的查询语义，不是可以内聚掉的实现细节。
   - **写侧 / 录入侧**：`Builder.addEdge(src, dst, kind uint8, extra uint32)` 里的 `kind`
     （`edgeOrdinary`/`edgeUseIface`/`edgeUseIfaceMethod`/`edgeUseNamedMethod`）只是一个
     "写入时选择塞进 `ordinaryEdges` 还是 `funcDemands`，以及 `funcDemands` 里该填哪个
     `Demand*` 值"的**内部 dispatch 参数**，本身不出现在 wire format / `FuncDemand.Kind`
     里（序列化进去的一直是 `Demand*`）。它和读侧的 `Demand*` 是 1:1 映射，是冗余的一层：
     唯一作用是让 `internal/meta` 内部选择编码位置。跨包调用方不再直接传 tag，而是调用专用的
     公开方法。

## Builder 对外方法列表（当前）

```go
// 符号登记 —— 不变
func (b *Builder) Sym(name string) LocalSymbol

// 普通符号引用（call / type use / global var ref ...）
// 来源：internal/build 对最终 LLVM IR 的事后扫描（metadata_edges.go）
func (b *Builder) AddOrdinaryEdge(src, dst LocalSymbol)

// src 把一个具体类型的值转换成接口
// 来源：ssa（语义层，MakeInterface / MakeInterfaceFromPtr）
func (b *Builder) AddIfaceUse(src, typ LocalSymbol)

// src 调用了 iface 的第 methodIndex 个方法（按声明顺序）
// 来源：ssa（语义层，Imethod）
func (b *Builder) AddIfaceMethodUse(src, iface LocalSymbol, methodIndex uint32)

// src 按常量方法名做了一次 MethodByName(methodName) 调用
// 来源：ssa（语义层，checkReflect）
func (b *Builder) AddNamedMethodEdge(src LocalSymbol, methodName string)

// src 触发了保守反射处理（动态 index/name，无法静态确定）
// 来源：ssa（语义层，checkReflect）
func (b *Builder) MarkReflect(src LocalSymbol)

// parent 类型结构性包含 child 类型（用于 UseIface 沿类型树传播）
// 来源：ssa（语义层，recordTypeChildren）
func (b *Builder) AddTypeChild(parent, child LocalSymbol)

// 为具体类型 typ 记录一个 ABI 方法槽位（定义方法实现）
// 来源：ssa（语义层，abiUncommonMethods）
func (b *Builder) AddMethodSlot(typ LocalSymbol, methodName string, mtype, ifn, tfn LocalSymbol)

// 为接口类型 iface 记录一个方法签名（定义接口方法集）
// 来源：ssa（语义层，recordInterfaceInfo）
func (b *Builder) AddIfaceMethod(iface LocalSymbol, methodName string, mtype LocalSymbol)

// 收口，产出制品 —— 不变
func (b *Builder) Build() (*PackageMeta, error)
```

**私有化范围**：`AddEdge` 方法本身已经改为 `addEdge`，不导出；它用来选分支的四个 tag 常量
`EdgeOrdinary`/`EdgeUseIface`/`EdgeUseIfaceMethod`/`EdgeUseNamedMethod` 也已经改为小写
`edgeOrdinary`/`edgeUseIface`/`edgeUseIfaceMethod`/`edgeUseNamedMethod`，不导出。
`AddOrdinaryEdge`/`AddIfaceUse`/`AddIfaceMethodUse`/`AddNamedMethodEdge` 内部各自调用私有的
`addEdge` 或直接写入对应 section。

**不受影响**：`DemandUseIface`/`DemandIfaceMethod`/`DemandNamedMethod`/`DemandReflectMethod`、
`FuncDemand.Kind` 字段、`GlobalSummary` 的全部查询方法——这些都不属于本次讨论范围。

## 各调用点状态

`ssa/interface.go`（两处）已经去掉对 edge tag 的直接引用：

```go
mb.AddIfaceMethodUse(mb.Sym(b.Func.Name()), intfSym, uint32(i))
mb.AddIfaceUse(mb.Sym(b.Func.Name()), mb.Sym(typeName))
```

`ssa/expr.go`（`checkReflect`）使用公开的反射/具名方法事实方法：

```go
mb.MarkReflect(mb.Sym(b.Func.Name()))
mb.AddNamedMethodEdge(mb.Sym(b.Func.Name()), v)
```

`ssa/abitype.go`（三处"定义类"事实）方法名/调用保持清晰，不经过 `addEdge`：

```go
mb.AddIfaceMethod(intfSym, mthName(f), mb.Sym(ftypName))                                                  // recordInterfaceInfo
mb.AddTypeChild(parent, mb.Sym(childName))                                                                // recordTypeChildren
mb.AddMethodSlot(mb.Sym(typeName), fullName, mb.Sym(mtypeName), mb.Sym(ifn.Name()), mb.Sym(tfn.Name()))   // abiUncommonMethods
```

`internal/build/metadata_edges.go`（生产代码里唯一的 ordinary edge 来源）已经改为：

```go
c.builder.AddOrdinaryEdge(c.builder.Sym(c.src), c.builder.Sym(dst))
```

测试代码也应优先使用公开 typed API。即使 `internal/meta/meta_test.go` 是 `package meta` 白盒测试，
一般 round-trip fixture 也不需要直接碰 `addEdge`/`edge*`。外部包测试更不能依赖私有 tag：

```go
b.AddOrdinaryEdge(main, allocZ)
b.AddIfaceUse(main, myType)
b.AddIfaceMethodUse(main, reader, 0)
```

## 待讨论 / 可选的后续小项（未决定，仅记录）

- `AddNamedMethodEdge` 要不要改名 `AddNamedMethodUse`，让三个"引用类"方法
  （`AddIfaceUse`/`AddIfaceMethodUse`/`AddNamedMethodUse`）后缀风格一致。非必须，纯命名细节。
- `internal/deadcode.Analyze` 目前直接吃 `*meta.GlobalSummary` 具体类型，测试要绕一圈
  `Builder.Build() → NewGlobalSummary` 才能造出 fixture（见 `internal/deadcode/analyze_test.go`
  的 `buildPackage`/`pkgBuilder`）。如果后续觉得这块测试成本高，可以考虑在 `internal/meta` 里
  加一个只含 `Analyze` 用到的那十来个方法的 `SummaryView` 接口，`*GlobalSummary` 自动满足，
  不影响 `internal/build` 的调用方式。这属于独立话题，和本次讨论的 `ssa`/`Builder` 边界无关，
  这里只是顺带记一笔。
