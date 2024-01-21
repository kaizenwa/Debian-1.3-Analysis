/*
 * fake ANSI C rename() function
 */

int
rename(old, new)
char *old;
char *new;
{
	(void) unlink(new);
	if (link(old, new) < 0)
		return(1);
	if (unlink(old) < 0) {
		/* oops... could link but not unlink... try to recover */
		(void) unlink(new);
		return(1);
	}
	return(0);
}
