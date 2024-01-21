/* @(#) bzero.c,v 1.2 1990/10/24 05:19:25 tron Exp */

/*
 * bzero(b, n)  -  zero out n bytes at b
 */
bzero(b, n)
    char *b;
    unsigned n;
{
    while (n > 0) {
	*b++ = '\0';
	--n;
    }
}
