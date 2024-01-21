#include "kiss.h"

int copyfiletofile (char *src, char *dest, CpFlags fl)
{
    FILE
	*inf,
	*outf;
    struct stat
	inbuf,
	outbuf;
    char
	destlink [FILENAMELEN];
    register int
	warnlevel = 0,
	nholes;
    struct utimbuf
	tm;
    
    if (fl.verbose)
	printf ("%s -> %s\n", src, dest);

    /* a. sourcefile must exist */
    if (stat (src, &inbuf))
	return (warning ("cannot stat input file \"%s\"", src));

    /* b. may destfile be overwritten? */
    if (! stat (dest, &outbuf))
    {
	if (inbuf.st_ino == outbuf.st_ino)
	    return (warning ("\"%s\" and \"%s\" are identical files",
			     src, dest));
	if (fl.interactive)
	{
	    printf ("%s: overwrite \"%s\" [y/?] ", progname, dest);
	    if (getinput (stdin) != 'y')
		return (0);
	}
    }

    /* c. copying links? */
    if (fl.noderef && islink (src, destlink))
    {
	if (symlink (destlink, dest))
	    return (warning ("failure creating symlink \"%s\" to \"%s\"",
			     destlink, dest));
	return (0);
    }

    /* d. simple file to file */
    if (! S_ISREG (inbuf.st_mode))
	return (warning ("\"%s\" -> \"%s\": cannot copy non-regular "
			 "files (yet)", src, dest));

    if (! (inf = fopen (src, "r")) )
	return (warning ("cannot open \"%s\" for reading", src));
    if (! (outf = fopen (dest, "w")) )
	return (warning ("cannot open \"%s\" for writing", dest));

    nholes = file2file (inf, outf);

    if (fl.verbose && nholes)
	printf ("%d hole(s) bigger than %d\n", nholes, HOLESIZE);

    fclose (inf);
	
    if (fchmod (fileno (outf), inbuf.st_mode))
	warnlevel += warning ("failure changing mode of \"%s\"", dest);

    fclose (outf);

    if (fl.preserve)
    {
	
	if (chown (dest, inbuf.st_uid, inbuf.st_gid))
	    warnlevel += warning ("failure changing ownership of \"%s\"",
				  dest);
	
	tm.actime = inbuf.st_atime;
	tm.modtime = inbuf.st_mtime;
	
	if (utime (dest, &tm))
	    warnlevel += warning ("failure changing timestamps of \"%s\"",
				  dest);
    }

    return (warnlevel);
}
