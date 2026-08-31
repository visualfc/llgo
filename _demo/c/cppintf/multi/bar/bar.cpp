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
