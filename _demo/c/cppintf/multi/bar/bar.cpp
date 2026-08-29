#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#define interface struct

interface ICalc {
	virtual double calc(double v) = 0;
};

interface IVal {
	virtual int val() = 0;
};

class MultiCallback : public ICalc, public IVal {
};

extern "C" void llgo_cppmintf_f(MultiCallback* cb) {
	int val = cb->val();
	double calc = cb->calc(2);
	printf("val: %d\ncalc(2): %lf\n", val, calc);
	fflush(stdout);
	if (val != 1 || fabs(calc - sqrt(2.0)) > 1e-12) {
		abort();
	}
}

#if defined(_WIN32) && defined(_M_IX86)
extern "C" double llgo_cppmintf_calc_cdecl(void* cb, double value);
extern "C" int llgo_cppmintf_val_cdecl(void* cb);

static double __thiscall llgo_cppmintf_calc_thiscall(ICalc* cb, double value) {
	return llgo_cppmintf_calc_cdecl(cb, value);
}

static int __thiscall llgo_cppmintf_val_thiscall(IVal* cb) {
	return llgo_cppmintf_val_cdecl(cb);
}

extern "C" void* llgo_cppmintf_calc_thunk() {
	return reinterpret_cast<void*>(&llgo_cppmintf_calc_thiscall);
}

extern "C" void* llgo_cppmintf_val_thunk() {
	return reinterpret_cast<void*>(&llgo_cppmintf_val_thiscall);
}
#endif
