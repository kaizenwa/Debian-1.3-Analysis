#include <stdio.h>
main() { printf("#define FLOORED_DIVISION %d\n", (-10 % 7) > 0 ? 1 : 0); }
