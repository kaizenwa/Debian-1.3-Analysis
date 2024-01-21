/*
 * Binary search algorithm, generalized from Knuth (6.2.1) Algorithm B.
 *
 * Written by J. S. Rugaber; rewritten by L. Rosler, Dept. 45175, August, 1981.
 */
char *
search(key, base, nel, width, compar)
char *key;			/* Key to be located */
char *base;			/* Beginning of table */
unsigned nel;			/* Number of elements in the table */
unsigned width;			/* Width of an element (bytes) */
int	(*compar)();		/* Comparison function */
{
	int two_width = width + width;
	char *last = base + width * (nel - 1); /* Last element in table */

	while (last >= base) {

		register char *p = base + width * ((last - base)/two_width);
		register int res = (*compar)(key, p);

		if (res == 0)
			return (p);	/* Key found */
		if (res < 0)
			last = p - width;
		else
			base = p + width;
	}
	return ((char *) 0);		/* Key not found */
}
