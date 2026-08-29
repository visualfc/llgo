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
