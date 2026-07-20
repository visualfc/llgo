#ifndef GO_TEST_PACKAGE
#error GO_TEST_PACKAGE must name the generated Go test main package
#endif

#ifdef __APPLE__
#define GO_SYMBOL(name) __asm__("_" name)
#else
#define GO_SYMBOL(name) __asm__(name)
#endif

extern void llgo_test_init(void) GO_SYMBOL(GO_TEST_PACKAGE ".init");
extern void llgo_test_run(void) GO_SYMBOL(GO_TEST_PACKAGE ".main");
extern int __llgo_argc;
extern char **__llgo_argv;

int main(int argc, char **argv) {
    __llgo_argc = argc;
    __llgo_argv = argv;
    llgo_test_init();
    llgo_test_run();
    return 0;
}
