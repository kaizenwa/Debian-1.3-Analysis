/** 
 **  BCOPY.C -- Emulation of the Unix BSTRING(3) library routines for VMS.
 **/

#define MOVC_MAX 65535


/* BCOPY -- Copies length bytes from string b1 to string b2.
 * Overlapping strings are handled correctly.
 */
bcopy (b1, b2, length)
	char	*b1, *b2;
	int	length;
{
	unsigned short	len;

	while (length > 0) {
	    len = (length < MOVC_MAX) ? length : MOVC_MAX;

	    lib$movc3 (&len, b1, b2);

	    length -= len;
	    b1     += len;
	    b2     += len;
	}
}


/* BCMP -- Compares byte string b1 against byte string b2, returning
 * zero if they are identical, non-zero otherwise.  Both strings are 
 * assumed to be length bytes long.
 */
bcmp (b1, b2, length)
	register char	*b1, *b2;
	int	length;
{
	register int	i;

	for (i = 0; i < length; i++)
	    if (*b1++ != *b2++)
		return (-1);
	return (0);
}


/* BZERO -- Zero fill a buffer.
 */
bzero (b, length)
	char	*b;
	int	length;
{
	unsigned short	src_len = 0;
	unsigned short	dst_len;
	char	null = '\000';


	while (length > 0) {
	    dst_len = (length < MOVC_MAX) ? length : MOVC_MAX;

	    lib$movc5 (&src_len, 0, &null, &dst_len, b);

	    length -= dst_len;
	    b      += dst_len;
	}
}
                                                           
                                 