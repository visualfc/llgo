; ModuleID = 'main'
source_filename = "main"

@main.hello = global ptr null
@"main.init$guard" = global ptr null

define void @main.init() {
_llgo_0:
  %0 = load i1, ptr @"main.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"main.init$guard", align 1
  store i8 104, ptr @main.hello, align 1
  store i8 101, ptr getelementptr inbounds (i8, ptr @main.hello, i64 1), align 1
  store i8 108, ptr getelementptr inbounds (i8, ptr @main.hello, i64 2), align 1
  store i8 108, ptr getelementptr inbounds (i8, ptr @main.hello, i64 3), align 1
  store i8 111, ptr getelementptr inbounds (i8, ptr @main.hello, i64 4), align 1
  store i8 10, ptr getelementptr inbounds (i8, ptr @main.hello, i64 5), align 1
  store i8 0, ptr getelementptr inbounds (i8, ptr @main.hello, i64 6), align 1
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  ret void
}

define void @main() {
_llgo_0:
  call void @main.init()
  %0 = alloca [4 x i64], align 8
  %1 = getelementptr inbounds i64, ptr %0, i64 0
  %2 = getelementptr inbounds i64, ptr %0, i64 1
  %3 = getelementptr inbounds i64, ptr %0, i64 2
  %4 = getelementptr inbounds i64, ptr %0, i64 3
  store i64 1, ptr %1, align 4
  store i64 2, ptr %2, align 4
  store i64 3, ptr %3, align 4
  store i64 4, ptr %4, align 4
  %5 = load [4 x i64], ptr %0, align 4
  br label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_2, %_llgo_0
  %6 = phi i64 [ 0, %_llgo_0 ], [ %10, %_llgo_2 ]
  %7 = phi i64 [ -1, %_llgo_0 ], [ %8, %_llgo_2 ]
  %8 = add i64 %7, 1
  %9 = icmp slt i64 %8, 4
  br i1 %9, label %_llgo_2, label %_llgo_3

_llgo_2:                                          ; preds = %_llgo_1
  %10 = add i64 %6, %8
  br label %_llgo_1

_llgo_3:                                          ; preds = %_llgo_1
  %11 = icmp eq i64 %6, 6
  br i1 %11, label %_llgo_4, label %_llgo_5

_llgo_4:                                          ; preds = %_llgo_3
  call void (ptr, ...) @printf(ptr @main.hello)
  br label %_llgo_5

_llgo_5:                                          ; preds = %_llgo_4, %_llgo_3
  ret void
}

declare void @printf(ptr, ...)
