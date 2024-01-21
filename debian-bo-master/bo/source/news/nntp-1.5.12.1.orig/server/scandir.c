#ifndef lint
static char	sccsid[] = "@(#)$Id: scandir.c,v 1.9 1994/11/05 06:04:10 sob Exp sob $";
#endif

#include "common.h"

/*
 * scan_dir -- scan the current directory for news articles,
 *	loading the article numbers into art_array.  Return
 *	number of articles loaded.
 *
 *	Parameters:	"low_msg", "high_msg" are the low
 *			and high messages numbers in this
 *			group; we ignore numbers outside this
 *			range.
 *
 *	Returns:	Number of articles loaded into
 *			array.
 *
 *	Side effects:	Changes "art_array".
 */

extern	int	intcmp();
extern char *malloc(), *realloc();

int
scan_dir(low_msg, high_msg)
int	low_msg, high_msg;
{
#ifdef DIRENT
	register struct dirent	*dirent;
#else
	register struct direct	*dirent;
#endif
	register DIR		*dirp;
	int			artnum;

	num_arts = 0;

	dirp = opendir(".");

	if (dirp == NULL)
		return (0);

	while ((dirent = readdir(dirp)) != NULL) {
		artnum = atoi(dirent->d_name);
		if (artnum == 0 || artnum < low_msg || artnum > high_msg)
			continue;
#ifdef DYNAMIC_ART_ARRAY
		/* Expand/allocate art_array elements as necessary */
		if (num_arts >= size_art_array) {
			size_art_array += 1024;
			if (art_array)
				art_array = (int *)realloc(art_array,
				    size_art_array * sizeof *art_array);
			else
				art_array = (int *)
				    malloc(size_art_array * sizeof *art_array);
			if (art_array == 0) {
#ifdef SYSLOG
				syslog(LOG_ERR,
				    "scan_dir(): malloc/realloc failed");
#endif
				num_arts = 0;
				size_art_array = 0;
				closedir(dirp);
				return(0);
			}
		}
#endif
		art_array[num_arts++] = artnum;

	}
	closedir(dirp);

	qsort((char *) art_array, num_arts, sizeof(int), intcmp);

	return (num_arts);
}


/*
 * intcmp -- compare to integers.
 *
 *	Parameters:	"x", "y" point to the integers to be compared.
 *
 *	Returns:	-1 if "x" is less than "y",
 *			0 if "x" equals "y", and
 *			1 if "x" is greater than "y".
 *
 *	Side effects:	None.
 */

int
intcmp(x, y)
register int	*x, *y;
{
	return (*x - *y);
}
