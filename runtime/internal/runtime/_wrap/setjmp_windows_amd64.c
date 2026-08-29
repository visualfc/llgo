/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#if defined(__x86_64__) || defined(_M_X64)

/*
 * _setjmpex stores the standard 256-byte Windows jump-buffer layout. CRT
 * longjmp behavior varies by linkage and may invoke the Windows virtual
 * unwinder, which cannot leave a vectored handler that interrupted generated
 * Go code. LLGo owns Go defer/panic unwinding, so restore the non-volatile
 * Win64 ABI state directly.
 */
__attribute__((naked, noinline, noreturn)) void llgo_longjmp(void *env,
                                                             int value) {
    __asm__ volatile(
        "movq %rcx, %r8\n\t"
        "movq 0x50(%r8), %r9\n\t"
        "movq 0x8(%r8), %rbx\n\t"
        "movq 0x10(%r8), %rsp\n\t"
        "movq 0x18(%r8), %rbp\n\t"
        "movq 0x20(%r8), %rsi\n\t"
        "movq 0x28(%r8), %rdi\n\t"
        "movq 0x30(%r8), %r12\n\t"
        "movq 0x38(%r8), %r13\n\t"
        "movq 0x40(%r8), %r14\n\t"
        "movq 0x48(%r8), %r15\n\t"
        "ldmxcsr 0x58(%r8)\n\t"
        "fldcw 0x5c(%r8)\n\t"
        "movdqa 0x60(%r8), %xmm6\n\t"
        "movdqa 0x70(%r8), %xmm7\n\t"
        "movdqa 0x80(%r8), %xmm8\n\t"
        "movdqa 0x90(%r8), %xmm9\n\t"
        "movdqa 0xa0(%r8), %xmm10\n\t"
        "movdqa 0xb0(%r8), %xmm11\n\t"
        "movdqa 0xc0(%r8), %xmm12\n\t"
        "movdqa 0xd0(%r8), %xmm13\n\t"
        "movdqa 0xe0(%r8), %xmm14\n\t"
        "movdqa 0xf0(%r8), %xmm15\n\t"
        "movl %edx, %eax\n\t"
        "testl %eax, %eax\n\t"
        "jne 1f\n\t"
        "incl %eax\n\t"
        "1:\n\t"
        "jmp *%r9");
}

#endif
