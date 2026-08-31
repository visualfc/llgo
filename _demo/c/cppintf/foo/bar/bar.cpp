#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#define interface struct

interface ICallback {
	virtual int val() = 0;
	virtual double calc(double v) = 0;
};

extern "C" void f(ICallback* cb) {
	int val = cb->val();
	double calc = cb->calc(2);
	printf("val: %d\ncalc(2): %lf\n", val, calc);
	fflush(stdout);
	if (val != 1 || fabs(calc - sqrt(2.0)) > 1e-12) {
		abort();
	}
}

void g(ICallback* cb) {
	f(cb);
}

#if defined(_WIN32)
extern "C" void llgo_cppintf_g(ICallback* cb) {
	g(cb);
}
#endif

#if defined(_WIN32) && defined(_M_IX86)
extern "C" int llgo_cppintf_val_cdecl(ICallback* cb);
extern "C" double llgo_cppintf_calc_cdecl(ICallback* cb, double value);

// A 32-bit MSVC vtable entry uses thiscall, whereas an LLGo C export uses
// cdecl. Keep that ABI-only adaptation at the C++ boundary; the Go callbacks
// continue to receive an ordinary explicit context pointer.
static int __thiscall llgo_cppintf_val_thiscall(ICallback* cb) {
	return llgo_cppintf_val_cdecl(cb);
}

static double __thiscall llgo_cppintf_calc_thiscall(ICallback* cb, double value) {
	return llgo_cppintf_calc_cdecl(cb, value);
}

extern "C" void* llgo_cppintf_val_thunk() {
	return reinterpret_cast<void*>(&llgo_cppintf_val_thiscall);
}

extern "C" void* llgo_cppintf_calc_thunk() {
	return reinterpret_cast<void*>(&llgo_cppintf_calc_thiscall);
}
#endif
