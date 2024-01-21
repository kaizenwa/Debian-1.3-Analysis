/*
 * This file contains pathfind(), fparse(), makename(), and smatch().
 */
#include <ctype.h>
#include "../h/gsupport.h"

hidden char *pathelem	Params((char *s, char *buf));
hidden char *tryfile	Params((char *buf, char *dir, char *name, char *extn));

/*
 * The following code is operating-system dependent [@filepart.01].
 *
 *  Define symbols for building file names.
 *  1. Prefix: the characters that terminate a file name prefix
 *  2. FileSep: the char to insert after a dir name, if any
 *  2. DefPath: the default IPATH/LPATH, if not null
 *  3. PathSep: the IPATH/LPATH separator, if not blank
 */

#if PORT
#define Prefix "/"
#define FileSep '/'
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA
#define Prefix "/:"
#define FileSep '/'
#endif					/* AMIGA */

#if ARM
#define Prefix ".:"
#define DefPath "Icon: Lib:Icon."
#endif					/* ARM */

#if ATARI_ST
#define Prefix "/:\\"
#endif					/* ATARI_ST */

#if MSDOS || OS2
#define Prefix "/:\\"
#define DefPath ";"
#if HIGHC_386
#define FileSep '\\'
#else					/* HIGHC_386 */
#define FileSep '/'
#endif					/* HIGHC_386 */
#endif					/* MSDOS || OS2 */

#if MACINTOSH
#define Prefix ":"
#define FileSep ':'
#if MPW || LWC
#define DefPath ":"
#endif					/* MPW || LSC */
#if MPW
#define PathSep ','
#endif					/* MPW */
#endif					/* MACINTOSH */

#if MVS
#define Prefix ""
#define FileSep '('
#if SASC
#define DefPath "iconlib ddn:::lib"
#endif					/* SASC */
#endif					/* MVS */

#if VM
#define Prefix ""
#endif					/* VM */

#if UNIX
#define Prefix "/"
#define FileSep '/'
#endif					/* UNIX */

#if VMS
#define Prefix "]:"
#endif					/* VMS */

/*
 * End of operating-system specific code.
 */

#ifndef DefPath
#define DefPath ""
#endif					/* DefPath */

#ifndef PathSep
#define PathSep ' '
#endif					/* PathSep */

/*
 * pathfind(buf,path,name,extn) -- find file in path and return name.
 *
 *  pathfind looks for a file on a path, begining with the current
 *  directory.  Details vary by platform, but the general idea is
 *  that the file must be a readable simple text file.  pathfind
 *  returns buf if it finds a file or NULL if not.
 *
 *  buf[MaxFileName] is a buffer in which to put the constructed file name.
 *  path is the IPATH or LPATH value, or NULL if unset.
 *  name is the file name.
 *  extn is the file extension (.icn or .u1) to be appended, or NULL if none.
 */
char *pathfind(buf, path, name, extn)
char *buf, *path, *name, *extn;
   {
   char *s;
   char pbuf[MaxFileName];

   if (tryfile(buf, (char *)NULL, name, extn))	/* try curr directory first */
      return buf;
   if (!path)				/* if no path, use default */
      path = DefPath;
   s = path;

#if SCCX_MX
   /* Avoids compiler warning */
   while( (s = pathelem(s, pbuf)) != 0)    /* for each path element */
#else
   while (s = pathelem(s, pbuf))	/* for each path element */
#endif					/* SCCX_MX */

      if (tryfile(buf, pbuf, name, extn))	/* look for file */
         return buf;
   return NULL;				/* return NULL if no file found */
   }

/*
 * pathelem(s,buf) -- copy next path element from s to buf.
 *
 *  Returns the updated pointer s.
 */
static char *pathelem(s, buf)
char *s, *buf;
   {
   char c;

   while (*s == PathSep)
      s++;
   if (!*s)
      return NULL;
   while ((c = *s) != '\0' && c != PathSep) {
      *buf++ = c;
      s++;
      }

#ifdef FileSep
   /*
    * We have to append a path separator here.
    *  Seems like makename should really be the one to do that.
    */
   if (!index(Prefix, buf[-1])) {	/* if separator not already there */
      *buf++ = FileSep;
      }
#endif					/* FileSep */

   *buf = '\0';
   return s;
   }

/*
 * tryfile(buf, dir, name, extn) -- check to see if file is readable.
 *
 *  The file name is constructed in buf from dir + name + extn.
 *  findfile returns buf if successful or NULL if not.
 */
static char *tryfile(buf, dir, name, extn)
char *buf, *dir, *name, *extn;
   {
   FILE *f;
   makename(buf, dir, name, extn);
   if ((f = fopen(buf, ReadText)) != NULL) {
      fclose(f);
      return buf;
      }
   else
      return NULL;
   }

/*
 * fparse - break a file name down into component parts.
 *  Result is a pointer to a struct of static pointers good until the next call.
 */
struct fileparts *fparse(s)
char *s;
   {
   static char buf[MaxFileName+2];
   static struct fileparts fp;
   int n;
   char *p, *q;

#if ARM
   static char buf[MaxFileName+2];
   static struct fileparts fp;
   char *p;
   char *ext = 0;
   char *extend = 0;
   char *dirend = 0;
   char *s1;
   char *bp = buf;

   /* First, skip any filing system prefix */
   s1 = index(s,':');
   if (s1 == NULL)
      s1 = s;
   else
      ++s1;

   /* Now, scan backwards through the filename, looking for dots.
    * Record the positions of the final two, for later use.
    */
   p = s1 + strlen(s1);
   fp.name = 0;
   
   while (--p > s1)
   {
         if (*p != '.')
            continue;

      if (fp.name == NULL)
      {
         fp.name = p + 1;
         extend = p;
      }
      else
      {
         ext = p + 1;
         dirend = p;
         break;
      }
   }

   /* This is the simple case. The filename is a simple name, with no
    * directory part. The extension is therefore null, and the directory
    * is just the filing system prefix, if any.
    */
   if (fp.name == NULL)
   {
      fp.name = s1;

      if (s1 == s)
      {
         fp.ext = "";
         fp.dir = "";
      }
      else
      {
         fp.ext = "";
         strncpy(buf, s, s1 - s);
         buf[s1-s] = '\0';
         fp.dir = buf;
      }

      return &fp;
   }

   /* Now worry about the more complicated cases. First, check the
    * supposed extension, to see if it is one of the valid cases,
    * SourceSuffix, U1Suffix, U2Suffix, or USuffix. For this code
    * to work, these four defined values must start with a dot, and
    * be all in lower case.
    */
   *buf = '.';
   bp = buf + 1;

   for (p = ext ? ext : s1; p < extend; ++p)
   {
      *bp++ = tolower(*p);
   }

   *bp++ = '\0';

   if (strcmp(buf,SourceSuffix) == 0 || strcmp(buf,U1Suffix) == 0
    || strcmp(buf,U2Suffix) == 0 || strcmp(buf,USuffix) == 0)
   {
      fp.ext = buf;
   }
   else
   {
      fp.ext = "";
      bp = buf;
      dirend = extend;
   }

   /* We now have the name and extension sorted out. So we just need
    * to copy the directory part into buf (at bp), and set fp.dir.
    */
   if (dirend == NULL)
   {
      if (s1 == s)
         fp.dir = "";
      else
      {
         fp.dir = bp;

         while (s < s1)
            *bp++ = *s++;

         *bp = '\0';
      }
   }
   else
   {
      fp.dir = bp;

      while (s <= dirend)
         *bp++ = *s++;

      *bp = '\0';
   }

   return &fp;

#else					/* ARM */

#if MVS
   static char extbuf [MaxFileName+2] ;

   p = index(s, '(');
   if (p) {
      fp.member = p+1;
      memcpy(extbuf, s, p-s);
      extbuf [p-s]  = '\0';
      s = extbuf;
   }
   else fp.member = s + strlen(s);
#endif					/* MVS */

   q = s;
   fp.ext = p = s + strlen(s);
   while (--p >= s) {
      if (*p == '.' && *fp.ext == '\0')
         fp.ext = p;
      else if (index(Prefix,*p)) {
         q = p+1;
         break;
         }
      }

   fp.dir = buf;
   n = q - s;
   strncpy(fp.dir,s,n);
   fp.dir[n] = '\0';
   fp.name = buf + n + 1;
   n = fp.ext - q;
   strncpy(fp.name,q,n);
   fp.name[n] = '\0';

#if VMS
   /* if a version is included, get separate extension and version */
   if (p = index(fp.ext, ';')) {
      fp.version = p;
      p = fp.ext;
      fp.ext = fp.name + n + 1;
      n = fp.version - p;
      strncpy(fp.ext, p, n);
      fp.ext[n] = '\0';
      }
   else
      fp.version = fp.name + n;		/* point version to '\0' */
#endif                                  /* VMS */

   return &fp;
#endif					/* ARM */
   }

/*
 * makename - make a file name, optionally substituting a new dir and/or ext
 */
char *makename(dest,d,name,e)
char *dest, *d, *name, *e;
   {
   struct fileparts fp;
   fp = *fparse(name);
   if (d != NULL)
      fp.dir = d;
   if (e != NULL)
      fp.ext = e;

#if ARM
   {
      char *p = (*fp.ext ? fp.ext + 1 : "");
      sprintf(dest, "%s%s%s%s", fp.dir, p, (*p ? "." : ""), fp.name);
   }

#else					/* ARM */

#if MVS
#if SASC
   {
      char *colons;
      colons = strstr(fp.name, ":::");
      if (colons) {
         memcpy(colons+1, e+1, 2);
         fp.ext = "";
      }
   }
#endif					/* SASC */
   if (*fp.member)
      sprintf(dest,"%s%s%s(%s", fp.dir, fp.name, fp.ext, fp.member);
   else
#endif					/* MVS */

   sprintf(dest,"%s%s%s",fp.dir,fp.name,fp.ext);
#endif					/* ARM */

   return dest;
   }

/*
 * smatch - case-insensitive string match - returns nonzero if they match
 */
int smatch(s,t)
char *s, *t;
   {
   char a, b;
   for (;;) {
      while (*s == *t)
         if (*s++ == '\0')
            return 1;
         else
            t++;
      a = *s++;
      b = *t++;
      if (isupper(a))  a = tolower(a);
      if (isupper(b))  b = tolower(b);
      if (a != b)
         return 0;
      }
   }
