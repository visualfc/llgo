#include <stdio.h>
#define interface struct

interface ICallback {
	virtual int val() = 0;
	virtual double calc(double v) = 0;
};

extern "C" void f(ICallback* cb) {
	printf("val: %d\ncalc(2): %lf\n", cb->val(), cb->calc(2));
	fflush(stdout);
}

void g(ICallback* cb) {
	f(cb);
}

#if defined(_WIN32)
extern "C" void llgo_cppintf_g(ICallback* cb) {
	g(cb);
}
#endif
