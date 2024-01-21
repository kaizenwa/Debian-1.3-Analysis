/*
 *      Directory Operations for Mac based on BSD 4.3   <macdir.h>
 *      By Jason Linhart, January 1997
 */

#ifndef _MACDIR_H
#define _MACDIR_H       1

#define NAME_MAX                255             /* # chars in a file name */

typedef long off_t;

struct dirent {
#ifdef COMMENT
        long            d_ino;
        off_t           d_off;
        unsigned short  d_reclen;
#endif
        char            d_name[NAME_MAX+1];
};

/* The internal is hidden from the user. */
typedef void DIR;

/* Open a directory stream on NAME.
   Return a DIR stream on the directory, or NULL if it could not be opened.  */extern DIR *opendir (const char *name);

/* Close the directory stream DIRP.
   Return 0 if successful, -1 if not.  */
extern int closedir (DIR * dirp);

/* Read a directory entry from DIRP.
   Return a pointer to a `struct dirent' describing the entry,
   or NULL for EOF or error.  The storage returned may be overwritten
   by a later readdir call on the same DIR stream.  */
extern struct dirent *readdir (DIR * dirp);

/* Rewind DIRP to the beginning of the directory.  */
extern void rewinddir (DIR * dirp);

/* Seek to position POS on DIRP.  */
extern void seekdir (DIR * dirp, off_t pos);

/* Return the current position of DIRP.  */
extern off_t telldir (DIR * dirp);

/* Fake stat to work with current directory entry */

#define stat(file_name,buf)             dirstat(file_name,buf)

struct stat {
        long st_mode;
        };

#define S_ISREG(mode)   (!(mode&0x10))

extern int dirstat(const char *file_name, struct stat *buf);

#endif /* macdir.h  */
