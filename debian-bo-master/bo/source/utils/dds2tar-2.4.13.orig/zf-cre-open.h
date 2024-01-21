
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 *
 * Copyright: GPL
 */

#define C_MODE 1		/* compressed */
#define T_MODE 0		/* transparent */

/*
 * Use popen and gzip to open a file with transparent compression.
 * The mode may be "r" or "w".
 */
extern FILE *zfopen(
			   char const *,	/* pathname */
			   int const,	/* compression, e.g. 0 or 1 */
			   char const *const	/* open mode, e.g. "r" or "w" */
);

/*
 * Open a pipe to a child process on a given file number, e.g. 1.
 *
 * Example of a pipe to tar -t:
 *                      static char * av[] = {"/bin/tar","-t",NULL};
 *                      creopen(1,0,"/bin/tar",av,NULL);
 */
extern int creopen(
			  int const,	/* file number of the parent */
			  int const,	/* file number of the child */
			  char const *const,	/* name of the program */
			  char const *const *	/* argv of the program */

);
extern int cclose(int const);

/*
 * Open a file on a given file number, e.g. 1.
 */
extern int reopen(
			 int const,	/* file number */
			 char const *const,	/* name of the file */
			 int const,	/* mode of the open */
			 int const	/* file mode */
);
