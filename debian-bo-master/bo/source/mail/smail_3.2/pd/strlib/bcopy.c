/* @(#) bcopy.c,v 1.2 1990/10/24 05:19:23 tron Exp */

/*
 * bcopy(b1, b2, n)  -  copy block b1 to b2 for n bytes.
 *
 * Caution:  This routine does not obey the full 4.3BSD bcopy semantics,
 *	     in that overlapping moves are not handled in any particularly
 *	     interresting manner.
 */
bcopy(b1, b2, n)
    char *b1;
    char *b2;
    unsigned n;
{
    while (n > 0) {
	*b2++ = *b1++;
	--n;
    }
}
