/* Declarations for URL handling.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: url.h,v 1.1.1.1.2.1 1997/02/15 19:23:19 hniksic Exp $ */

#ifndef URL_H
#define URL_H

/* URL separator (for findurl) */
#define URL_SEPARATOR "!\"#'(),>`{}|<>"

/* Default port definitions */
#define DEFAULT_HTTP_PORT 80
#define DEFAULT_FTP_PORT 21

/* Tha smaller value of the two. */
#define MINVAL(x, y) ((x) < (y) ? (x) : (y))

/* ASCII char -> HEX digit */
#define ASC2HEXD(x) (((x) >= '0' && (x) <= '9') ?               \
		     ((x) - '0') : (toupper(x) - 'A' + 10))

/* HEX digit -> ASCII char */
#define HEXD2ASC(x) (((x) >= 0 && (x) <= 9) ?           \
		     ((x) + '0') : ((x) - 10 + 'A'))

/* A list of unsafe characters for encoding, as per RFC1738.  '@' and
   ':' (not listed in RFC) were added because of user/password
   encoding, and \033 because of safe printing. */
#define URL_UNSAFE " <>\"#%{}|\\^~[]`@:\033"

/* If the string contains unsafe characters, duplicate it with
   encode_string, otherwise just copy it with strdup. */
#define CLEANDUP(x) (contains_unsafe(x) ? encode_string(x) : nstrdup(x))

/* Just another ugly macro (defines a static local char *). */
#define URL_CLEANSE(s)                                          \
   do { if (contains_unsafe(s)) { static char *tmp;             \
   tmp = encode_string(s); free(s); s = tmp; }} while (0);

/* Is a directory "."? */
#define ISDOT(x) ((*(x) == '.') && (!*(x + 1)))
/* Is a directory ".."? */
#define ISDDOT(x) ((*(x) == '.') && (*(x + 1) == '.') && (!*(x + 2)))

#define USE_PROXY(u) (opt.use_proxy && getproxy((u)->proto)             \
                      && no_proxy_match((u)->host,                      \
                                        (const char **)opt.no_proxy))

/* Structure containing info on a protocol. */
typedef struct proto {
   char *name;
   uerr_t ind;
   unsigned short port;
} proto_t;

/* Structure containing info on a URL. */
typedef struct _urlinfo {
   char *url;                   /* Unchanged URL */
   uerr_t proto;                /* URL protocol */
   char *host;                  /* Extracted hostname */
   unsigned short port;
   char *path, *dir, *file;     /* Path, as well as dir and file
                                   (properly decoded) */
   char *user, *passwd;         /* For FTP */
   struct _urlinfo *proxy;      /* The exact string to pass to proxy
                                   server. */
   char *referer;               /* The source from which the request
                                   URI was obtained. */
   char *local;                 /* The local filename of the URL
                                   document. */
} urlinfo;

enum uflags {
   URELATIVE     = 0x0001,      /* Is URL relative? */
   UNOPROTO      = 0x0002,      /* Is URL without a protocol? */
   UABS2REL      = 0x0004,      /* Convert absolute to relative? */
   UREL2ABS      = 0x0008       /* Convert relative to absolute? */
};

/* A structure that defines the whereabouts of a URL, i.e. its
   position in an HTML document, etc. */
typedef struct _urlpos {
   char *url;                   /* URL */
   char *local_name;            /* Local file to which it was saved. */
   enum uflags flags;           /* Various flags. */
   int pos, size;               /* Rekative position in the buffer. */
   struct _urlpos *next;        /* Next struct in list. */
} urlpos;


/* Function declarations */

int skip_url PARAMS((const char *));

int contains_unsafe PARAMS((const char *));
void decode_string PARAMS((char *));
char *encode_string PARAMS((const char *));

urlinfo *newurl PARAMS((void));
void freeurl PARAMS((urlinfo *, int));
uerr_t urlproto PARAMS((const char *));
int skip_proto PARAMS((const char *));
int has_proto PARAMS((const char *));
int skip_uname PARAMS((const char *));

uerr_t parseurl PARAMS((const char *, urlinfo *, int));
uerr_t parse_uname PARAMS((const char *, char **, char **));
void parse_dir PARAMS((const char *, char **, char **));
char *str_url PARAMS((const urlinfo *, int));
int url_equal PARAMS((const char *, const char *));

const char *findurl PARAMS((const char *, int, int *));
urlpos *get_urls_file PARAMS((const char *));
urlpos *get_urls_html PARAMS((const char *, const char *, int));
void free_urlpos PARAMS((urlpos *));

int mkalldirs PARAMS((const char *));
char *mkstruct PARAMS((const urlinfo *));
char *url_filename PARAMS((const urlinfo *));
char *unique_name PARAMS((const char *, int));
char *construct PARAMS((const char *, const char *, int, int));
void opt_url PARAMS((urlinfo *));

char *getproxy PARAMS((uerr_t));
int no_proxy_match PARAMS((const char *, const char **));

void convert_links PARAMS((const char *, urlpos *));
char *construct_relative PARAMS((const char *, const char *));
urlpos *add_url PARAMS((urlpos *, const char *, const char *));

#endif /* URL_H */
