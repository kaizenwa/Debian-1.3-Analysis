/* @(#) bcmp.c,v 1.2 1990/10/24 05:19:19 tron Exp */

/*
 * bcmp(b1, b2, n)  -  compare block b1 to b2 for n bytes, return 0 if same
 */
bcmp(b1, b2, n)
    char *b1;
    char *b2;
    unsigned n;
{
    while (n > 0) {
	if (*b1++ != *b2++) {
	    return 1;
	}
	--n;
    }
    return 0;
}
