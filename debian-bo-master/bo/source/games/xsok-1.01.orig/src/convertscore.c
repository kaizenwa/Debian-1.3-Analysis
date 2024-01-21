#include <stdio.h>
#include <stdlib.h>

/* convert a savegame file from the alpha version to the beta version */

int main(void) {
    long len;
    unsigned char p[64];
    unsigned char s[10000];
    memset(p, 0xff, sizeof(p));
    fread(p, 32, 1, stdin);
    
    len = fread(s, 1, 10000, stdin);

    memcpy(p+28, p+12, 4);	/* stored move */
    memset(p+20, 0, 8);
    p[23] = 1;		/* assume it's finished */
    p[27] = s[7];	/* level */
    memcpy(p+48, p+12, 4);	/* bookmark where started */
    if (s[0] == 'C')
	s[7] = 'x';
    else
	s[7] = '\0';
    fwrite(p, 1, 64, stdout);
    fwrite(s, 1, len, stdout);
    return 0;
}
