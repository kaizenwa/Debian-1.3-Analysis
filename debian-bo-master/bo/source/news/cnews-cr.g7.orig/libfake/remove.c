/*
 * fake ANSI C remove() function
 */

int
remove(file)
char *file;
{
	if (unlink(file) < 0)
		return(1);
	else
		return(0);
}
