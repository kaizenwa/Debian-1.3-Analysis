#include <stdio.h>

main()
{
    int i=0;

    printf("  0 [");
    putchar (i);
    for (i = 1; i < 256; i++) {
        if ((i / 16) * 16 == i) {
            putchar (']');
            putchar ('\n');
            printf("%3d [", i);
        }
        putchar (i);
    }
    putchar (']');
    putchar ('\n');

    exit(0);

} /* main */

