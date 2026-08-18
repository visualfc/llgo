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

#if defined(_WIN32) && defined(__aarch64__)

/*
 * LLGo implements Go defer/panic unwinding itself. UCRT longjmp additionally
 * invokes the Windows virtual unwinder, which rejects ARM64 assembly frames
 * without .pdata (notably libffi closures). Preserve exactly the non-volatile
 * Windows ARM64 ABI state so LLGo can leave such transparent foreign frames.
 *
 * The offsets follow the 192-byte UCRT ARM64 jmp_buf layout used by the
 * runtime's target-size declarations. The first two words are reserved.
 */
__attribute__((naked, noinline, returns_twice)) int llgo_setjmp(void *env) {
    __asm__ volatile(
        "str xzr, [x0, #0]\n\t"
        "str xzr, [x0, #8]\n\t"
        "stp x19, x20, [x0, #16]\n\t"
        "stp x21, x22, [x0, #32]\n\t"
        "stp x23, x24, [x0, #48]\n\t"
        "stp x25, x26, [x0, #64]\n\t"
        "stp x27, x28, [x0, #80]\n\t"
        "stp x29, x30, [x0, #96]\n\t"
        "mov x2, sp\n\t"
        "str x2, [x0, #112]\n\t"
        "mrs x2, fpcr\n\t"
        "str w2, [x0, #120]\n\t"
        "mrs x2, fpsr\n\t"
        "str w2, [x0, #124]\n\t"
        "stp d8, d9, [x0, #128]\n\t"
        "stp d10, d11, [x0, #144]\n\t"
        "stp d12, d13, [x0, #160]\n\t"
        "stp d14, d15, [x0, #176]\n\t"
        "mov w0, wzr\n\t"
        "ret");
}

__attribute__((naked, noinline, noreturn)) void llgo_longjmp(void *env,
                                                             int value) {
    __asm__ volatile(
        "ldp x19, x20, [x0, #16]\n\t"
        "ldp x21, x22, [x0, #32]\n\t"
        "ldp x23, x24, [x0, #48]\n\t"
        "ldp x25, x26, [x0, #64]\n\t"
        "ldp x27, x28, [x0, #80]\n\t"
        "ldp x29, x30, [x0, #96]\n\t"
        "ldr x2, [x0, #112]\n\t"
        "mov sp, x2\n\t"
        "ldr w2, [x0, #120]\n\t"
        "msr fpcr, x2\n\t"
        "ldr w2, [x0, #124]\n\t"
        "msr fpsr, x2\n\t"
        "ldp d8, d9, [x0, #128]\n\t"
        "ldp d10, d11, [x0, #144]\n\t"
        "ldp d12, d13, [x0, #160]\n\t"
        "ldp d14, d15, [x0, #176]\n\t"
        "cmp w1, #0\n\t"
        "csinc w0, w1, wzr, ne\n\t"
        "ret");
}

#endif
