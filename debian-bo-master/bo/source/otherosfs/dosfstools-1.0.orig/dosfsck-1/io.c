/* io.c  -  Virtual disk input/output */

/* Written 1993 by Werner Almesberger */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include "dosfsck.h"
#include "common.h"
#include "io.h"


typedef struct _change {
    void *data;
    unsigned int pos;
    int size;
    struct _change *next;
} CHANGE;


static CHANGE *changes,*last;
static int fd,did_change = 0;


void fs_open(char *path,int rw)
{
    if ((fd = open(path,rw ? O_RDWR : O_RDONLY)) < 0)
	pdie("open %s",path);
    changes = last = NULL;
    did_change = 0;
}


void fs_read(unsigned int pos,int size,void *data)
{
    CHANGE *walk;
    int got;

    if (lseek(fd,pos,0) != pos) pdie("Seek to %d",pos);
    if ((got = read(fd,data,size)) < 0) pdie("Read %d bytes at %d",size,pos);
    if (got != size) die("Got %d bytes instead of %d at %d",got,size,pos);
    for (walk = changes; walk; walk = walk->next)
	if (walk->pos < pos+size && walk->pos+walk->size > pos)
	    if (walk->pos < pos)
		memcpy(data,(char *) walk->data+pos-walk->pos,min(size,
		  walk->size-pos+walk->pos));
	    else memcpy((char *) data+walk->pos-pos,walk->data,min(walk->size,
		  size+pos-walk->pos));
}


int fs_test(unsigned int pos,int size)
{
    void *scratch;
    int okay;

    if (lseek(fd,pos,0) != pos) pdie("Seek to %d",pos);
    scratch = alloc(size);
    okay = read(fd,scratch,size) == size;
    free(scratch);
    return okay;
}


void fs_write(unsigned int pos,int size,void *data)
{
    CHANGE *new;
    int did;

    if (write_immed) {
	did_change = 1;
	if (lseek(fd,pos,0) != pos) pdie("Seek to %d",pos);
	if ((did = write(fd,data,size)) == size) return;
	if (did < 0) pdie("Write %d bytes at %d",size,pos);
	die("Wrote %d bytes instead of %d at %d",did,size,pos);
    }
    new = alloc(sizeof(CHANGE));
    new->pos = pos;
    memcpy(new->data = alloc(new->size = size),data,size);
    new->next = NULL;
    if (last) last->next = new;
    else changes = new;
    last = new;
}


static void fs_flush(void)
{
    CHANGE *this;
    int size;

    while (changes) {
	this = changes;
	changes = changes->next;
	if (lseek(fd,this->pos,0) != this->pos)
	    fprintf(stderr,"Seek to %d failed: %s\n  Did not write %d bytes.\n",
	      this->pos,strerror(errno),this->size);
	else if ((size = write(fd,this->data,this->size)) < 0)
		fprintf(stderr,"Writing %d bytes at %d failed: %s\n",this->size,
		  this->pos,strerror(errno));
	    else if (size != this->size)
		    fprintf(stderr,"Wrote %d bytes instead of %d bytes at %d."
		      "\n",size,this->size,this->pos);
	free(this->data);
	free(this);
    }
}


int fs_close(int write)
{
    CHANGE *next;
    int changed;

    changed = !!changes;
    if (write) fs_flush();
    else while (changes) {
	    next = changes->next;
	    free(changes->data);
	    free(changes);
	    changes = next;
	}
    if (close(fd) < 0) pdie("closing file system");
    return changed || did_change;
}


int fs_changed(void)
{
    return !!changes || did_change;
}
