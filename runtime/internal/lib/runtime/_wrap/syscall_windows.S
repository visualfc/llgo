/*
 * Copyright 2025 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style license available at
 * https://go.dev/LICENSE.
 *
 * Adapted from internal/runtime/syscall/windows/asm_windows_386.s,
 * asm_windows_amd64.s, and asm_windows_arm64.s for LLGo's C ABI, including
 * explicit COFF unwind data where the platform uses table-based unwinding.
 */

/*
 * Variable-width Windows API call bridge. The layout of llgo_windows_syscall
 * is kept in sync with windowsSyscall in syscall_windows_llgo.go.
 */

#if defined(__x86_64__)

	.text
	.globl llgo_windows_syscall
	.def llgo_windows_syscall;
	.scl 2;
	.type 32;
	.endef
	.seh_proc llgo_windows_syscall
llgo_windows_syscall:
	subq $360, %rsp
	.seh_stackalloc 360
	.seh_endprologue
	movq %rcx, 336(%rsp)

	/* SetLastError(0) through the TEB. */
	movq %gs:0x30, %r10
	movl $0, 0x68(%r10)

	/* The first four slots are Win64 shadow space. Remaining arguments are
	 * already in their final outgoing-stack positions. */
	movq 8(%rcx), %r10
	movq 16(%rcx), %r11
	xorl %eax, %eax
.Lcopy_x64:
	cmpq %r10, %rax
	jae .Lload_x64
	movq (%r11,%rax,8), %rdx
	movq %rdx, (%rsp,%rax,8)
	incq %rax
	jmp .Lcopy_x64

.Lload_x64:
	cmpq $4, %r10
	jb .Lless4_x64
	movq 24(%rsp), %r9
	movq %r9, %xmm3
.Lless4_x64:
	cmpq $3, %r10
	jb .Lless3_x64
	movq 16(%rsp), %r8
	movq %r8, %xmm2
.Lless3_x64:
	cmpq $2, %r10
	jb .Lless2_x64
	movq 8(%rsp), %rdx
	movq %rdx, %xmm1
.Lless2_x64:
	cmpq $1, %r10
	jb .Lcall_x64
	movq 0(%rsp), %rcx
	movq %rcx, %xmm0

.Lcall_x64:
	movq 336(%rsp), %r11
	movq 0(%r11), %rax
	callq *%rax

	movq 336(%rsp), %rcx
	movq %rax, 24(%rcx)
	movq %xmm0, 32(%rcx)
	movq %gs:0x30, %r10
	movl 0x68(%r10), %eax
	movq %rax, 40(%rcx)
	addq $360, %rsp
	retq
	.seh_endproc

#elif defined(__aarch64__)

	.text
	.globl llgo_windows_syscall
	.def llgo_windows_syscall;
	.scl 2;
	.type 32;
	.endef
llgo_windows_syscall:
	.seh_proc llgo_windows_syscall
	/* 34 stack arguments plus x19/x20 and frame/link registers. */
	sub sp, sp, #304
	.seh_stackalloc 304
	stp x19, x20, [sp, #272]
	.seh_save_regp x19, 272
	stp x29, x30, [sp, #288]
	.seh_save_fplr 288
	.seh_endprologue
	add x29, sp, #288
	mov x19, x0

	/* SetLastError(0) through the Windows ARM64 TEB register. */
	str wzr, [x18, #0x68]

	ldr x9, [x19, #8]
	ldr x12, [x19, #16]
	cmp x9, #8
	b.ls .Lload_arm64
	sub x10, x9, #8
	mov x11, #0
.Lcopy_arm64:
	add x13, x12, #64
	ldr x13, [x13, x11, lsl #3]
	str x13, [sp, x11, lsl #3]
	add x11, x11, #1
	cmp x11, x10
	b.lo .Lcopy_arm64

.Lload_arm64:
	cmp x9, #8
	b.lo .Lless8_arm64
	ldr x7, [x12, #56]
.Lless8_arm64:
	cmp x9, #7
	b.lo .Lless7_arm64
	ldr x6, [x12, #48]
.Lless7_arm64:
	cmp x9, #6
	b.lo .Lless6_arm64
	ldr x5, [x12, #40]
.Lless6_arm64:
	cmp x9, #5
	b.lo .Lless5_arm64
	ldr x4, [x12, #32]
.Lless5_arm64:
	cmp x9, #4
	b.lo .Lless4_arm64
	ldr x3, [x12, #24]
.Lless4_arm64:
	cmp x9, #3
	b.lo .Lless3_arm64
	ldr x2, [x12, #16]
.Lless3_arm64:
	cmp x9, #2
	b.lo .Lless2_arm64
	ldr x1, [x12, #8]
.Lless2_arm64:
	cmp x9, #1
	b.lo .Lcall_arm64
	ldr x0, [x12]

.Lcall_arm64:
	ldr x20, [x19]
	blr x20
	str x0, [x19, #24]
	str xzr, [x19, #32]
	ldr w0, [x18, #0x68]
	str x0, [x19, #40]

	.seh_startepilogue
	ldp x19, x20, [sp, #272]
	.seh_save_regp x19, 272
	ldp x29, x30, [sp, #288]
	.seh_save_fplr 288
	add sp, sp, #304
	.seh_stackalloc 304
	.seh_endepilogue
	ret
	.seh_endfunclet
	.seh_endproc

#elif defined(__i386__)

	.text
	.globl _llgo_windows_syscall
	.def _llgo_windows_syscall;
	.scl 2;
	.type 32;
	.endef
_llgo_windows_syscall:
	pushl %ebp
	movl %esp, %ebp
	pushl %ebx
	pushl %esi
	pushl %edi

	/* The LLGo C ABI passes the windowsSyscall pointer on the stack. */
	movl 8(%ebp), %ebx

	/* SetLastError(0) through the x86 TEB. */
	movl $0, %fs:0x34

	/* Both stdcall and cdecl receive their arguments on the stack. The
	 * explicit stack restoration after the call accommodates either cleanup
	 * convention, matching the Go runtime's x86 bridge. */
	movl 4(%ebx), %ecx
	testl %ecx, %ecx
	jz .Lcall_x86
	movl %ecx, %eax
	shll $2, %eax
	subl %eax, %esp
	movl %esp, %edi
	movl 8(%ebx), %esi
	cld
	rep movsl

.Lcall_x86:
	call *0(%ebx)
	leal -12(%ebp), %esp

	movl %eax, 12(%ebx)
	movl %edx, 16(%ebx)
	movl %fs:0x34, %eax
	movl %eax, 20(%ebx)

	popl %edi
	popl %esi
	popl %ebx
	popl %ebp
	retl

#else
#error "LLGo Windows SyscallN currently supports 386, amd64, and arm64"
#endif
