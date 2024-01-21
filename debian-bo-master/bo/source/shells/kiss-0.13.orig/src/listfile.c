/* Major/minor version numbers: by Glenn Ammons <ammons@access.digex.net>
   thanks for the contrib..
*/   

#include "kiss.h"

#define DEV_MINOR(x) (x & 0xff)
#define DEV_MAJOR(x) ((x & 0xff00) >> 8)

int listfile (char *name, LsFlags fl)
{
    struct stat
	statbuf;
    char
	buf [FILENAMELEN] = { '\0' },
	tmp [FILENAMELEN];
    struct passwd
	*pwd;
    struct group
	*grp;
    struct tm
	*loctime;

    if (lstat (name, &statbuf) && stat (name, &statbuf))
	return (warning ("can't stat \"%s\"", name));

    if (fl.longoutput)
    {
	sprintf (buf, "%c%c%c%c%c%c%c%c%c%c %2d",
		 S_ISLNK (statbuf.st_mode) ? 'l' 
		 : S_ISREG (statbuf.st_mode) ? '-'
		 : S_ISDIR (statbuf.st_mode) ? 'd'
		 : S_ISCHR (statbuf.st_mode) ? 'c'
		 : S_ISBLK (statbuf.st_mode) ? 'b'
		 : S_ISFIFO (statbuf.st_mode) ? 'p'
		 : S_ISSOCK (statbuf.st_mode) ? 's'
		 : '?',
		 S_IRUSR & statbuf.st_mode ? 'r' : '-',
		 S_IWUSR & statbuf.st_mode ? 'w' : '-',
		 (S_IXUSR & statbuf.st_mode
                  && S_ISUID & statbuf.st_mode) ? 's' 
		 : (S_IXUSR & statbuf.st_mode
		    && S_ISUID ^ statbuf.st_mode) ? 'x'
		 : (S_IXUSR ^ statbuf.st_mode
		    && S_ISUID & statbuf.st_mode) ? 'S' : '-',
		 S_IRGRP & statbuf.st_mode ? 'r' : '-',
		 S_IWGRP & statbuf.st_mode ? 'w' : '-',
		 (S_IXGRP & statbuf.st_mode
                  && S_ISGID & statbuf.st_mode) ? 's' 
		 : (S_IXGRP & statbuf.st_mode
		    && S_ISGID ^ statbuf.st_mode) ? 'x'
		 : (S_IXGRP ^ statbuf.st_mode
		    && S_ISGID & statbuf.st_mode) ? 'S' : '-',
		 S_IROTH & statbuf.st_mode ? 'r' : '-',
		 S_IWOTH & statbuf.st_mode ? 'w' : '-',
		 (S_IXOTH & statbuf.st_mode
                  && S_ISVTX & statbuf.st_mode) ? 't' 
		 : (S_IXOTH & statbuf.st_mode
		    && S_ISVTX ^ statbuf.st_mode) ? 'x'
		 : (S_IXOTH ^ statbuf.st_mode
		    && S_ISVTX & statbuf.st_mode) ? 'T' : '-',
		 (int) statbuf.st_nlink);

	if ( (pwd = getpwuid (statbuf.st_uid)) )
	    sprintf (tmp, " %9s", pwd->pw_name);
	else
	    sprintf (tmp, " %9d", statbuf.st_uid);
	strcat (buf, tmp);

	if ( (grp = getgrgid (statbuf.st_gid)) )
	    sprintf (tmp, " %9s", grp->gr_name);
	else
	    sprintf (tmp, " %9d",statbuf.st_gid);
	strcat (buf, tmp);

	if (S_ISCHR (statbuf.st_mode) 
	    || S_ISBLK (statbuf.st_mode))
	  sprintf (tmp, " %4ld,%4ld", 
		   (long) DEV_MAJOR(statbuf.st_rdev),
		   (long) DEV_MINOR(statbuf.st_rdev));
	else
	  sprintf (tmp, " %9ld", (long) statbuf.st_size);
	strcat (buf, tmp);

	loctime = localtime (&statbuf.st_mtime);
	sprintf (tmp, " %2.2d/%2.2d/%2.2d %2.2d:%2.2d ",
		 loctime->tm_mon, loctime->tm_mday, loctime->tm_year,
		 loctime->tm_hour, loctime->tm_min);
	
	strcat (buf, tmp);
    }

    strcat (buf, name);

    if (fl.listtype)
    {
	if (S_ISDIR (statbuf.st_mode))
	    strcat (buf, "/");
	else if (S_ISFIFO (statbuf.st_mode))
	    strcat (buf, "=");
	else if (S_ISLNK (statbuf.st_mode))
	    strcat (buf, "@");
    }

    if (fl.oneperline)
	puts (buf);
    else
	listoutput (buf);

    return (0);
}
