/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include "../config.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include <locale.h>

#include "getdir.h"
#include "cathegory.h"

/*****************************************************************************
 *
 * Read directory in a buffer
 *
 */
static unsigned char *match_func(unsigned char *path, unsigned char *str);
static int sort_func(const void *a, const void *b);
static void sort_names_and_files(unsigned char **ni,
				 unsigned char **fi, int cnt);

int read_dir_in_buffer(unsigned char *root,	/* root path */
		       unsigned char *path,	/* path relative to root */
		       unsigned char ***filep,
		       unsigned char ***dirp,
		       unsigned char ***namep,
		       int *files, int *dirs, int *current) {
  DIR *dp=NULL;
  unsigned char **fi = NULL, **di = NULL, **ni = NULL, **new;
  unsigned char *buf, *name, *curstr;
  size_t fi_size = 0, fi_inc = 0, fic=0;
  size_t di_size = 0, di_inc = 0, dic=0;
  size_t ni_size = 0, ni_inc = 0, nic=0;
  size_t bufp, pathlen;
  struct dirent *dir;
  struct stat st;
  int save, i, j;

  save = errno;
  errno = 0;
  if (!path) path="";

  if (!*dirp) *dirs=0; /* be sure */

  buf = alloca((pathlen=strlen(path))+(bufp=strlen(root))+256);
  strcpy(buf, root);
  if (buf[bufp-1]!='/') buf[bufp++]='/';
  strcpy(buf+bufp, path);
  bufp+=pathlen;
  if (buf[bufp-1]!='/') buf[bufp++]='/';
  buf[bufp]=0;

  if (!(dp=opendir(buf))) return 0;

  if (!make_room_for_more(dic, &di_size, &di_inc, &di)) goto error;

  if (pathlen) {
    di[dic]=strdup(path);
  } else {
    di[dic]=strdup(NO_CATHEGORY);
  }
  if (!di[dic]) { errno=ENOMEM; goto error; }
  curstr=di[dic];
  dic++;

  while ((dir = readdir(dp))) {

    if (!make_room_for_more(dic, &di_size, &di_inc, &di)) break;
    if (!make_room_for_more(fic, &fi_size, &fi_inc, &fi)) break;
    if (!make_room_for_more(nic, &ni_size, &ni_inc, &ni)) break;

    strcpy(&buf[bufp], dir->d_name);

    if (stat(buf, &st)) st.st_mode = 0;
    
    if (S_ISDIR(st.st_mode)) {
      name=dir->d_name;
      if (strcmp(name, ".")) {
	if (!strcmp(name, "..")) {
	  if (pathlen) {
	    unsigned char *bar;
	    di[dic]=strdup(path);
	    if (!di[dic]) { errno=ENOMEM; goto error; }
	    if (di[dic][pathlen-1]=='/') di[dic][pathlen-1]=0;
	    if ((bar=strrchr(di[dic],'/'))) {
	      *bar=0;
	    } else {
	      free(di[dic]);
	      di[dic]=strdup(NO_CATHEGORY);
	      if (!di[dic]) { errno=ENOMEM; goto error; }
	    }
	    dic++;
	  }
	} else {
	  di[dic]=malloc(strlen(name)+pathlen+2);
	  if (!di[dic]) { errno=ENOMEM; goto error; }
	  if (pathlen) {
	    strcpy(di[dic], path);
	    if (di[dic][pathlen-1]!='/') {
	      di[dic][pathlen]='/';
	      strcpy(di[dic]+pathlen+1, name);
	    } else {
	      strcpy(di[dic], name);
	    }
	  } else {
	    strcpy(di[dic], name);
	  }
	  dic++;
	}
      }
    } else {
      if ((name=match_func(buf, dir->d_name))) {
	ni[nic] = name;
	fi[fic] = strdup(dir->d_name);
	if (!ni[nic]) { errno=ENOMEM; goto error; }
	nic++;
	if (!fi[fic]) { errno=ENOMEM; goto error; }
	fic++;
      }
    }
  }

  if (errno) goto error;

  closedir(dp);
  errno = save;

  sort_names_and_files(ni, fi, nic); /* nic==fic (well, hopefully...) */

  curstr=strdup(curstr);

  /* Merge list of directories (==cathegories) with old list */

  /*
   * avoid double entries
   */
  if (dirp) {
    for (i=0; i<(*dirs); i++) {
      for (j=0; j<dic; j++) {
	if (!strcmp((*dirp)[i],di[j])) {
	  free(di[j]);
	  if (j+1<dic)
	    memmove(&di[j], &di[j+1], (dic-j-1)*sizeof(unsigned char *));
	  dic--;
	  break;
	}
      }
    }
  }

  /*
   * Do the actual merging
   */
  if (dic+(*dirs)) {
    new=malloc((dic+(*dirs)) * sizeof(unsigned char *));
    if (*dirs) memcpy(new, *dirp, (*dirs) * sizeof(unsigned char *));
    if (dic) memcpy(&new[(*dirs)], di, dic * sizeof(unsigned char *));
    if (*dirp) free(*dirp);
    *dirp = new;
    *dirs = dic+(*dirs);
    *current=(*dirs)-1;
    for (i=0; i<(*dirs); i++) {
      if (!strcmp(curstr, (*dirp)[i])) { *current=i; break; }
    }
    qsort(*dirp, *dirs, sizeof (unsigned char *), sort_func);
  } else {
    if (*dirp) free_dir_buffer(dirp,  *dirs);
    *dirp = NULL;
    *dirs = 0;
    *current = 0;
  }

  free(curstr);

  if (*filep) free_dir_buffer(filep, *files);
  if (*namep) free_dir_buffer(namep, *files);
  *filep=fi; *namep=ni; *files=fic;
  return 1;

error:
  save = errno;
  if (dp) closedir(dp);
  while (fic > 0) free (fi[--fic]);
  while (dic > 0) free (di[--dic]);
  while (nic > 0) free (ni[--nic]);
  if (fi) free(fi);
  if (di) free(di);
  if (ni) free(ni);
  errno = save;
  return 0;
}

void free_dir_buffer(unsigned char ***files, int count) {
  int n;
  if (*files) {
    for (n=0; n<count; n++) if ((*files)[n]) free((*files)[n]);
    free(*files);
    *files=NULL;
  }
}

/*
 * Return name of the media as a malloc'ed string if the file is in proper
 * format or NULL if not.
 */
static unsigned char *match_func(unsigned char *path, unsigned char *str) {
  if (strlen(str)!=8) return NULL;
  while(*str) {
    if (!(((*str>='0') && (*str<='9')) ||
	  ((*str>='a') && (*str<='f')))) return NULL;
    str++;
  }

  return read_media_name(path);
}

unsigned char *read_media_name(unsigned char *path) {
  FILE *f;
  unsigned char buf[512];
  int len;

  if ((f=fopen(path,"r"))) {
    while (fgets(buf,512,f)) {
      if (!strncmp(buf, "DTITLE=", 7)) {
	fclose(f);
	if (buf[(len=strlen(buf)-1)]=='\n') buf[len]=0;
	return strdup(buf+7);
      }
    }
    fclose(f);
  }
  return NULL;
}

    
static int sort_func(const void *a, const void *b) {
  return strcoll(((unsigned char **) a)[0], ((unsigned char **) b)[0]);
}


int make_room_for_more(size_t count, size_t *size, size_t *inc,
		       unsigned char ***buf) {
  int oldsize=*size;
  if ((count+5) >= (*size)) {
    unsigned char **new;
    if ((*size) == 0) {
      *size = 10; 
    } else {
      if ((*size)<500) {
	*size = *inc = (*size)<<1;
      } else {
	*size += *inc;
      }
    }
    new = (unsigned char **)realloc((*buf), (*size)*sizeof(unsigned char *));
    if (!new) {
      errno = ENOMEM;
      return 0;
    }
    memset(&new[oldsize], 0, ((*size)-oldsize)*sizeof(unsigned char *));
    *buf = new;
  }
  return 1;
}

/*
 * Well, this code is braindead, but it works...
 *
 * Sort the media names in alphabethical order and don't forget to
 * sort the file names using the media name order, too.
 * To accomplish this, I first place pairs of filenames and medianames
 * into a structure array, sort this array by medianames and then split
 * it up again into two arrays as the layer above needs it.
 *
 */

struct braindead {
  unsigned char *media;
  unsigned char *file;
};

static int stupid_sort_func(const void *a, const void *b) {
  return strcoll(((struct braindead *)a)->media,
		 ((struct braindead *)b)->media);
}


static void sort_names_and_files(unsigned char **ni,
				 unsigned char **fi, int cnt) {
  struct braindead *stupid;
  int i;

  if (!cnt) return;

  stupid=alloca(cnt * sizeof(struct braindead));

  for (i=0; i<cnt; i++) {
    stupid[i].media=ni[i];
    stupid[i].file =fi[i];
  }

  qsort(stupid, cnt, sizeof(struct braindead), stupid_sort_func);

  for (i=0; i<cnt; i++) {
    ni[i]=stupid[i].media;
    fi[i]=stupid[i].file;
  }
}







