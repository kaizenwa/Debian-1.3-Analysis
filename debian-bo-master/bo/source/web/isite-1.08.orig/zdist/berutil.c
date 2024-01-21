/*
(c)1992 OCLC Online Computer Library Center, Inc., 6565 Frantz Road, Dublin,
Ohio 43017-0702.  OCLC is a registered trademark of OCLC Online Computer
Library Center, Inc.

NOTICE TO USERS:  The BER Utilities ("Software") has been developed by OCLC
Online Computer Library Center, Inc.  Subject to the terms and conditions set
forth below, OCLC grants to user a perpetual, non-exclusive, royalty-free
license to use, reproduce, alter, modify, and create derivative works from
Software, and to sublicense Software subject to the following terms and
conditions:

SOFTWARE IS PROVIDED AS IS.  OCLC MAKES NO WARRANTIES, REPRESENTATIONS, OR
GUARANTEES WHETHER EXPRESS OR IMPLIED REGARDING SOFTWARE, ITS FITNESS FOR ANY
PARTICULAR PURPOSE, OR THE ACCURACY OF THE INFORMATION CONTAINED THEREIN.

User agrees that :1) OCLC shall have no liability to user arising therefrom,
regardless of the basis of the action, including liability for special,
consequential, exemplary, or incidental damages, including lost profits,
even if it has been advised of the possibility thereof; and :2) user will
indemnify and hold OCLC harmless from any claims arising from the use of
the Software by user's sublicensees.

User shall cause the copyright notice of OCLC to appear on all copies of
Software, including derivative works made therefrom.
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef PORTINFO
#include "portinfo.h"
#endif
#include "berutil.h"

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#ifdef DEBUG
static int level;
#endif

int berutil_ByteOrder;	/* 0 for LOW_TO_HIGH, 1 for HIGH_TO_LOW */

static int bld_nod(CHAR far *s, DATA_DIR far *dir, INT4 *tot_len,
	DATA_DIR far *parent);

/*
	builds a directory to an asn.1 record pointed to by s.  calls bld_nod
	to do all the real work.
*/
int bld_dir(PUCHR s, PDATA_DIR dir)
{
    INT4     len=0;

    if(!dir)
    {
#ifdef DEBUG
        printf("in bld_dir: null directory passed in\n");
#endif
        return FALSE;
    }

#ifdef DEBUG
    printf("in bld_dir: dir=%x, dir->block=%x, dir->block->dir=%x\n", dir,
        dir->block, dir->block->dir);
#endif
    dinit(dir, 0, 0);
    dir=dir->block->dir;
    dir->block->next_dir=dir;
#ifdef DEBUG
    level=0;
#endif
    return bld_nod(s, dir, &len, NULL_DIR);
}

/*
    bld_nod is called with a pointer to an asn.1 field, a pointer to a INT4
    so that it can return the total length of the asn.1 field, and a pointer
    to the node that is to be this node's parent.  if build_node discovers
    that the field has subfields, then it calls itself to handle the subfields,
    passing the node that it created as the parent for the new_node nodes
*/
static int bld_nod(CHAR far *ptr, DATA_DIR far *dir, INT4 *tot_len,
	DATA_DIR far *parent)
{
    CHAR     c, far *s;
    DATA_DIR far *child, far *last, far *node;
    int      lenlen, taglen;
    INT4     field_len, sub_len, tlen;
#ifdef DEBUG
    CHAR far *t;
    INT4     i;
#endif
	s=ptr;
#ifdef DEBUG
    printf("in bld_nod: dir=%x, dir->block=%x, dir->block->next_dir=%x\n",
        dir, dir->block, dir->block->next_dir);
    printf("    dir->block->last_dir=%x\n", dir->block->last_dir);
#endif
    if(dir->block->next_dir > dir->block->last_dir)
        if(!dmake_bigger(dir))
        {
#ifdef DEBUG
            printf("in bld_nod: 1) dir filled up\n");
            level--;
#endif
            return FALSE;
        }
    node=dir->block->next_dir++;
    if(dir->block->next_dir <= dir->block->last_dir)
        dir->block->next_dir->block=dir->block;
#ifdef DEBUG
    printf("node=%x ", node);
    if(parent)
        printf("last_dir=%x ", parent->block->last_dir);
    printf("level=%d\n", ++level);
#endif
    if(parent)
        parent->count++;

    node->parent=parent;
    node->next = NULL_DIR;
    node->Class = *s>>6;   /* the first byte of the asn.1 field */
    node->form=(*s>>5)&1;  /* contains the class and ASN1_CONSTRUCTED */
                           /* information */

    c=(*s++)&0x1f;         /* the first byte of the tag */

    *tot_len=1;
    if(c<0x1f)             /* if the tag is less than 31, then it */
        node->fldid=c;     /* is fully contained in the one byte */

    else                   /* otherwise we keep looking at bytes */
    {                      /* until we find 1 with the sign bit off */
        node->fldid=0;     /* catenating the last 7 bits of each byte */
        c= *s++;
        while(c>0x80)
        {
            *tot_len+=1;
            node->fldid+=c&0x7F;
            node->fldid<<=7;
            c= *s++;
        }
        *tot_len+=1;
        node->fldid+=c;
    }
    taglen= (int)*tot_len;

    lenlen=get_len(&field_len, s, 0x7fffffff);
    if(field_len == -1)  /* indeterminate length */
        if(IsCompleteBER(ptr, 0x7fffffff, &field_len))
        {
#ifdef DEBUG
            printf("indeterminate length determined to be %ld\n",
                field_len);
#endif
	    field_len-=(taglen+lenlen);
	    *tot_len+=lenlen+field_len;
	    field_len-=2;  /* don't include trailing nulls in loop later */
	}
	else
	{
#ifdef DEBUG
	    printf("we don't think we have a complete record\n");
#endif
	    return FALSE;
	}
    else
        *tot_len+=lenlen+field_len;
    s+=lenlen;

#ifdef DEBUG
    printf("class=%d, form=%d, tag=%d, len=%ld\n", node->Class, node->form,
        node->fldid, field_len);
#endif

    if(node->form==ASN1_CONSTRUCTED)
    {                    /* field has subfields.  set up and recurse */
        node->count=0;
        if(node->block->next_dir > node->block->last_dir)
            if(!dmake_bigger(node))
            {
                node->ptr.child=NULL_DIR;
#ifdef DEBUG
                printf("in bld_nod: 2) dir filled up\n");
                level--;
#endif
                return FALSE;
            }
        node->ptr.child=node->block->next_dir;
        tlen=field_len;
#ifdef DEBUG
        printf("in blddir: tlen=%ld\n", tlen);
#endif
        last=NULL_DIR;
        while(tlen>0)  /* we know how INT4 the field is.  loop until */
        {              /* the total length of the subfields equals */
                       /* the field length.  don't exceed dir size */
            if(node->block->next_dir > node->block->last_dir)
                if(!dmake_bigger(node))
                {
                    node->ptr.child=NULL_DIR;
#ifdef DEBUG
                    printf("in bld_nod: 3) dir filled up\n");
                    level--;
#endif
                    return FALSE;
                }
            child=node->block->next_dir;
            child->prev=last;
            if(last)
                last->next=child;
            if(!bld_nod(s, dir, &sub_len, node))
            {
#ifdef DEBUG
                level--;
#endif
                return FALSE;
            }
            last=child;
            tlen-=sub_len;
#ifdef DEBUG
            printf("in blddir: tlen=%ld, sub_len=%ld\n", tlen, sub_len);
#endif
            s+=sub_len;
        }
        if(last)  /* this should always be the case */
            last->next=NULL_DIR;
        else
        {
#ifdef DEBUG
            printf("in bld_dir: a ASN1_CONSTRUCTED field of length ");
            printf("<=0 detected\n    field_len=%ld\n", field_len);
#endif
            node->ptr.child=NULL_DIR;
        }

        if(tlen)  /* exceeded dir size */
        {
#ifdef DEBUG
            printf("in blddir: error, non-zero tlen=%ld\n", tlen);
            level--;
#endif
            return FALSE;
        }
    }

    else  /* no subfields, so just point at the data */
    {
        node->count=field_len;
        node->ptr.data=s;
#ifdef DEBUG
        printf("data='");
        for(i=0, t=s; i<field_len; i++, t++)
            fputc(*t, stdout);
        printf("'\n");
#endif
    }

#ifdef DEBUG
    level--;
#endif
    return TRUE;
}

CHAR far *bld_rec(PDATA_DIR dir, INT4 *len)
{
    CHAR far *record;

    *len=rec_len(dir);  /* calculate the length of the record */

#ifdef DEBUG
    printf("in bld_rec: about to malloc %ld bytes\n", *len);
#endif
    record=(CHAR far*)malloc((unsigned)*len);
			       /* allocate space for the record */
    if(record)
        asm_rec(dir, record);  /* pull the pieces of the record together */

    return record;
}

INT4 asm_rec(PDATA_DIR dir, PUCHR record)
{
    int      i;
    CHAR far *t;
    DATA_DIR far *child;
    INT4     len, tlen, tot_len;

#ifdef DEBUG
    printf("in asm_rec: adding field %d\n", (int)dir->fldid);
#endif

    if(dir->fldid<31)
        *record++ = (CHAR)(dir->fldid + dir->Class*64 + dir->form*32);
    else
    {
        *record++ = (CHAR)(31 + dir->Class*64 + dir->form*32);
        if(dir->fldid<128)
            *record++ = (CHAR)dir->fldid;
        else
        {
            *record++ = (CHAR)(128 + dir->fldid/128);
            *record++ = (CHAR)(dir->fldid%128);
        }
    }
    tot_len=TAG_LEN(dir);

    len=dir->length;
    if(len<128)
    {
        *record++ =(CHAR)len;
        tot_len+=1;
    }
    else
    {
        *record=(CHAR)(LEN_LEN(dir)-1);   /* number of bytes in length */
        tot_len+= *record+1;
        t=record+*record;  /* go to end of length field and load backwards */
        for(i=0; i<(int)*record; i++)
        {
            *t-- =(CHAR)(len & 0xFF);
            len>>=8;
        }
        record+= *record+1;
        *t+=128;  /* t points at length byte.  turn on bit 8 */
    }

    if(dir->form==ASN1_CONSTRUCTED)
    {
        child=dir->ptr.child;
        while(child)
        {
            tlen=asm_rec(child, record);
            record+=tlen;
            tot_len+=tlen;
            child=child->next;
        }
    }
    else
    {
#ifdef DEBUG
        printf("    which is %ld bytes INT4\n", dir->count);
#endif
        COPY(record, dir->ptr.data, (unsigned)dir->count);
        tot_len+=dir->count;
    }

    return tot_len;
}

DATA_DIR far *daddnum(DATA_DIR far *msg, unsigned fldid, CHAR Class,
		      CHAR far *ptr, int len)
{
    DATA_DIR far *dir, far *next_dir;
    INT4     number;

    if(!msg || msg->form==ASN1_PRIMITIVE)
        return NULL_DIR;

    if(msg->block->next_dir > msg->block->last_dir)
        if(!dmake_bigger(msg))
            return NULL_DIR;
    next_dir=msg->block->next_dir++;
    if(msg->block->next_dir <= msg->block->last_dir)
        next_dir->block->next_dir->block=next_dir->block;

    msg->count++;
    if(msg->ptr.child)
    {
        dir=msg->ptr.child;
        while(dir->next)
            dir=dir->next;
        dir->next=next_dir;
        next_dir->prev=dir;
    }
    else
    {
        msg->ptr.child=next_dir;
        next_dir->prev=NULL_DIR;
    }

    next_dir->block=msg->block;
    next_dir->parent=msg;
    next_dir->next = NULL_DIR;
    next_dir->user = 0;

    if(len==1)
        number= *ptr;
    else
        if(len==2)
            number= *(short*)ptr;
        else
            number= *(INT4*)ptr;

    ptr=(CHAR*)&number;
if(berutil_ByteOrder == 1)
    next_dir->number=number;
else {
	CHAR far *t;
	t=(CHAR*)(&next_dir->number);
	t[ 0 ]= ptr[ 3 ];
	t[ 1 ]= ptr[ 2 ];
	t[ 2 ]= ptr[ 1 ];
	t[ 3 ]= ptr[ 0 ];
}
/*
#if (NATIVE_ORDER&PDP_ORDER)==PDP_ORDER
    CHAR far *t;
    t=(CHAR*)(&next_dir->number);
    t[ 0 ]= ptr[ 1 ];
    t[ 1 ]= ptr[ 0 ];
    t[ 2 ]= ptr[ 3 ];
    t[ 3 ]= ptr[ 2 ];
#endif
*/
    next_dir->ptr.data=(CHAR*)(&next_dir->number);
    next_dir->count=sizeof(INT4);
    if(next_dir->ptr.data[ 0 ]=='\0')
        while(next_dir->count>1 && next_dir->ptr.data[ 0 ]=='\0' &&
            next_dir->ptr.data[ 1 ]<128)
        {
            next_dir->ptr.data++;
            next_dir->count--;
        }
	else
        if(next_dir->ptr.data[ 0 ]==0xff)
            while(next_dir->count>1 && next_dir->ptr.data[ 0 ]==0xff &&
                next_dir->ptr.data[ 1 ]>127)
            {
                next_dir->ptr.data++;
                next_dir->count--;
            }

    next_dir->fldid=fldid;
    next_dir->Class=Class;
    next_dir->form=ASN1_PRIMITIVE;

    return next_dir;
}

DATA_DIR far *dalloc(int numdirs)
{
    BLK_DIR  far *blk_dir;
    DATA_DIR far *dir=NULL_DIR;

/* Added by Mike Gursky */
    if(numdirs < 1)
	numdirs = 1;
/* end */
    if( (blk_dir=(BLK_DIR far*)malloc(sizeof(BLK_DIR)))==NULL )
    {
#ifdef DEBUG
        printf("in dalloc: malloc of blk_dir failed\n");
#endif
        return NULL_DIR;
    }
    if( (blk_dir->block_array=(DATA_DIR far* far*)
        malloc(sizeof(CHAR far*)))==NULL )
    {
#ifdef DEBUG
        printf("in dalloc: malloc of block_array failed\n");
#endif
        return NULL_DIR;
    }

    if( (blk_dir->next_dir = blk_dir->dir = blk_dir->block_array[ 0 ] =
        dir = (DATA_DIR far*)
        malloc((unsigned)(numdirs*sizeof(DATA_DIR))))==NULL )
    {
        free((CHAR far*)blk_dir);
#ifdef DEBUG
        printf("in dalloc: malloc of %d nodes failed\n", numdirs);
#endif
        return NULL_DIR;
    }

    blk_dir->last_dir = &dir[ numdirs-1 ];
    blk_dir->num_blocks=1;
    blk_dir->num_nodes=numdirs;
    dir->block=blk_dir;
#ifdef DEBUG
    printf("in dalloc: blk_dir=%lx, dir=%lx, next_dir=%lx, last_dir=%lx\n",
        blk_dir, dir, dir->block->next_dir, dir->block->last_dir);
#endif
    return dir;
}

int dfree(PDATA_DIR dir)
{
    BLK_DIR  far *blk_dir;

    if(!dir)
        return FALSE;
    blk_dir=dir->block;
    while(blk_dir->num_blocks-- > 0)
        free((CHAR far*)blk_dir->block_array[ blk_dir->num_blocks ]);
    free((CHAR far*)blk_dir->block_array);
    free((CHAR far*)blk_dir);
    return TRUE;
}

DATA_DIR far *dmake(unsigned fldid, CHAR Class, int numdirs)
{
    DATA_DIR far *dir;

    dir=dalloc(numdirs);
    if(dir)
        dinit(dir, fldid, Class);

    return dir;
}


DATA_DIR far *dinit(DATA_DIR far *dir, unsigned fldid, CHAR Class)
{
    BLK_DIR  far *blk_dir;

#ifdef DEBUG
    printf("dir=%lx\n", dir);
    printf("dir->block=%lx\n", dir->block);
    printf("dir->block->dir=%lx\n", dir->block->dir);
#endif
    blk_dir=dir->block;
    dir=blk_dir->dir;
/* fix by Mike Gursky */
    blk_dir->next_dir = &dir->block->dir[ 1 ];
    blk_dir->last_dir = &dir[ blk_dir->num_nodes-1 ];
    if(blk_dir->next_dir <= blk_dir->last_dir)
        blk_dir->next_dir->block = blk_dir;
/* end */
/* old
    blk_dir->next_dir = &dir->block->dir[ 1 ];
    blk_dir->next_dir->block=blk_dir;
    blk_dir->last_dir = &dir[ blk_dir->num_nodes-1 ];
*/
    while(blk_dir->num_blocks > 1)
    {
        blk_dir->num_blocks--;
        free((CHAR far*)blk_dir->block_array[ blk_dir->num_blocks ]);
    }

    dir->parent = dir->prev = dir->next = dir->ptr.child = NULL_DIR;
    dir->count = dir->user = 0;

    dir->fldid=fldid;
    dir->Class=Class;
    dir->form=ASN1_CONSTRUCTED;

    return dir;
}

DATA_DIR far *daddchar(DATA_DIR far *msg, unsigned fldid, CHAR Class,
    CHAR far *ptr, INT4 len)
{
    DATA_DIR far *dir, far *next_dir;

    if(!msg || msg->form==ASN1_PRIMITIVE)
        return NULL_DIR;

    if(msg->block->next_dir > msg->block->last_dir)
        if(!dmake_bigger(msg))
            return NULL_DIR;
    next_dir=msg->block->next_dir++;
    if(msg->block->next_dir <= msg->block->last_dir)
        next_dir->block->next_dir->block=next_dir->block;

    msg->count++;
    if(msg->ptr.child)
    {
        dir=msg->ptr.child;
        while(dir->next)
            dir=dir->next;
        dir->next=next_dir;
        next_dir->prev=dir;
    }
    else
    {
        msg->ptr.child=next_dir;
        next_dir->prev=NULL_DIR;
    }

    next_dir->block=msg->block;
    next_dir->parent=msg;
    next_dir->next = NULL_DIR;
    next_dir->user = 0;

    next_dir->ptr.data=ptr;
    next_dir->count = len;

    next_dir->fldid=fldid;
    next_dir->Class=Class;
    next_dir->form=ASN1_PRIMITIVE;

    return next_dir;
}

DATA_DIR far *daddtag(DATA_DIR far *msg, unsigned fldid, CHAR Class)
{
    DATA_DIR far *dir, far *next_dir;

    if(!msg || msg->form==ASN1_PRIMITIVE)
        return NULL_DIR;

    if(msg->block->next_dir > msg->block->last_dir)
        if(!dmake_bigger(msg))
            return NULL_DIR;
    next_dir=msg->block->next_dir++;
    if(msg->block->next_dir <= msg->block->last_dir)
        next_dir->block->next_dir->block=next_dir->block;

    msg->count++;
    if(msg->ptr.child)
    {
        dir=msg->ptr.child;
        while(dir->next)
            dir=dir->next;
        dir->next=next_dir;
        next_dir->prev=dir;
    }
    else
    {
        msg->ptr.child=next_dir;
        next_dir->prev=NULL_DIR;
    }

    next_dir->block=msg->block;
    next_dir->parent=msg;
    next_dir->ptr.child = next_dir->next = NULL_DIR;
    next_dir->count = next_dir->user = 0;

    next_dir->fldid=fldid;
    next_dir->Class=Class;
    next_dir->form=ASN1_CONSTRUCTED;

    return next_dir;
}

DATA_DIR far *dadddir(PDATA_DIR msg, PDATA_DIR new_nodedir)
{
    DATA_DIR far *dir;

    if(!msg || msg->form==ASN1_PRIMITIVE)
        return NULL_DIR;

    msg->count++;
    if(msg->ptr.child)
    {
        dir=msg->ptr.child;
        while(dir->next)
            dir=dir->next;
        dir->next=new_nodedir;
        new_nodedir->prev=dir;
    }
    else
    {
        msg->ptr.child=new_nodedir;
        new_nodedir->prev=NULL_DIR;
    }

    new_nodedir->parent=msg;

    return new_nodedir;
}

int ddeldir(PDATA_DIR msg)
{
    DATA_DIR far *prev;

    if(!msg || !msg->parent)
        return FALSE;

    msg->parent->count--;

    /* find prev */
    prev=msg->parent->ptr.child;
    if(prev==msg)
        msg->parent->ptr.child=msg->next;
    else
    {
        while(prev->next!=msg)
            prev=prev->next;
        prev->next=msg->next;
    }
    return TRUE;
}

int dmake_bigger(PDATA_DIR dir)
{
    BLK_DIR  far *blk_dir;

    if(!dir)
        return FALSE;
    blk_dir=dir->block;
    blk_dir->num_blocks++;
#ifdef DEBUG
    printf("in dmbigger1: dir = %lx blk_dir = %lx ", dir,blk_dir);
    printf("block_array = %lx num_blocks = %d\n",
        blk_dir->block_array,blk_dir->num_blocks);
#endif
    if( (blk_dir->block_array=
        (DATA_DIR far* far*)realloc((CHAR far*)blk_dir->block_array,
        blk_dir->num_blocks*sizeof(CHAR far*)))==NULL )
    {
#ifdef DEBUG
        printf("in dmbigger: realloc of block_array failed\n");
#endif
        return FALSE;
    }
    if( (blk_dir->block_array[ blk_dir->num_blocks-1 ]=
        (DATA_DIR far*)malloc(blk_dir->num_nodes*sizeof(DATA_DIR)))==NULL )
	{
#ifdef DEBUG
        printf("in dmbigger: malloc of %d new_node nodes failed\n",
            blk_dir->num_nodes);
#endif
        return FALSE;
	}
	blk_dir->next_dir=blk_dir->block_array[ blk_dir->num_blocks-1 ];
	blk_dir->next_dir->block=blk_dir;
	blk_dir->last_dir=blk_dir->next_dir+blk_dir->num_nodes-1;
#ifdef DEBUG
    printf("in dmbigger2: dir = %lx blk_dir = %lx ", dir,blk_dir);
    printf("block_array = %lx num_blocks = %d\n",
        blk_dir->block_array,blk_dir->num_blocks);
#endif
	return TRUE;
}

int dtag_found(DATA_DIR far *dir, unsigned fldid, CHAR Class)
{
	DATA_DIR far *child;

    if( !dir || !(child=dir->ptr.child) )
        return FALSE;

    for(child=dir->ptr.child; child; child=child->next)
        if(child->fldid==fldid && child->Class==Class)
            return TRUE;
    return FALSE;
}

INT4 dgetnum(PDATA_DIR dir)
{
	INT4 count;
    UINT4 i=0;

    if(dir->ptr.data[ 0 ]>127)
	for(count=0; count<4-dir->count; count++)
	{
        i|=0xff;
        i<<=8;
	}
    for(count=0; count<dir->count; count++)
    {
        i|=dir->ptr.data[ count ];
        if(count<dir->count-1)
            i<<=8;
    }
    return (INT4)i;
}

INT4 asn1len(CHAR far *s)
{
    CHAR c;
    int  len_len, tag_len=1;
    INT4 field_len;

    c=(CHAR)((*s++)&0x1f);        /* the first byte of the tag */

    if(c>=0x1f)                   /* if the tag is greater than 31, then we */
    {                             /* keep looking at bytes until we find 1 */
        c= *s++;                  /* with it's sign bit off */
        tag_len++;
        while(c>0x80)
        {
            c= *s++;
            tag_len++;
        }
    }

    len_len=get_len(&field_len, s, 0x7fffffff);
    return (INT4)(tag_len + len_len + field_len);
}

void hex_dir(PDATA_DIR dir, int level)
{
    hex_dirf(dir, level, stdout);
}

void hex_dirf(PDATA_DIR dir, int level, FILE *file)
{
	CHAR far *data;
	DATA_DIR far *child;
    int      i, linelen, tlen;

    static char *tabs=NULL;
    static char hex1[  ]={
'0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
'1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
'2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
'3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
'4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
'5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
'6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
'7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
'8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
'9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9',
'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B',
'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C',
'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D',
'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E',
'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
};
    static CHAR hex2[  ]={
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
};

    if(!tabs)
    {
        tabs=(char *)malloc(100);
        memset(tabs, ' ', 99);
        tabs[99]='\0';
    }

    if(!dir)
    {
        fprintf(file, "hex_dir passed a NULL directory pointer\n");
        return;
    }

    tabs[ level*4 ]='\0';

	fprintf(file, "%stag=%d, Class=%d, form=%d, count=%d\n", tabs,
        dir->fldid, dir->Class, dir->form, dir->count);

    tabs[ level*4 ]=' ';

    if(dir->form==ASN1_CONSTRUCTED)
        for(child=dir->ptr.child; child; child=child->next)
            hex_dirf(child, level+1, file);
    else
    {
        tabs[ (level+1)*4 ]='\0';

        if(dir->ptr.data)
        {
            fprintf(file, "%sdata=", tabs);
            linelen=79-strlen(tabs)-5;
            tlen=(int)dir->count;
            data=dir->ptr.data;
            while(tlen>0)
            {
                for(i=0; i<tlen && i<linelen; i++)
                    fprintf(file, "%c",
                        isprint(data[ i ])?data[ i ]:'.');
                fprintf(file, "\n%s     ", tabs);
                for(i=0; i<tlen && i<linelen; i++)
                    fprintf(file, "%c", hex1[ data[ i ] ]);
                fprintf(file, "\n%s     ", tabs);
                for(i=0; i<tlen && i<linelen; i++)
                    fprintf(file, "%c", hex2[ data[ i ] ]);
                tlen-=linelen;
                data+=linelen;
                if(tlen>0)
                    fprintf(file, "\n%s     ", tabs);
                else
                    fprintf(file, "\n");
            }
        }
        else
            fprintf(file, "%sdata=NONE\n", tabs);
    }

    tabs[ (level+1)*4 ]=' ';
}

INT4 rec_len(PDATA_DIR dir)
{
	DATA_DIR far *child;

    if(!dir)
        return 0;
    dir->length=0;
    if(dir->form==ASN1_CONSTRUCTED)
    {
        for(child=dir->ptr.child; child; child=child->next)
            dir->length+=rec_len(child);
#ifdef DEBUG
        printf("in rec_len: total length for field %d is %ld\n",
            dir->fldid, dir->length);
#endif
    }
    else
    {
        dir->length=dir->count;
#ifdef DEBUG
        printf("in rec_len: length of field %d is %ld\n",
            dir->fldid, dir->length);
#endif
    }

    return dir->length + LEN_LEN(dir) + TAG_LEN(dir);
}

int IsCompleteBER(CHAR far *ptr, INT4 len, INT4 *remainder)
{
    int  lenlen, tag, taglen;
    INT4 fieldlen, headerlen;

    *remainder=0;
    if(!len)
        return FALSE;

    if( (taglen=get_tag(&tag, ptr, len))==0)  /* no tag yet */
        return FALSE;

    if( (lenlen=get_len(&fieldlen, ptr+taglen, len-taglen))==0)
        return FALSE;  /* no len yet */

    headerlen=taglen+lenlen;
    if(lenlen==1 && fieldlen == -1)  /* indefinite length */
    {
        INT4 fieldlen, totlen=0;
        /* loop through the subfields and see if they are complete */
        for(ptr+=headerlen, len-=headerlen; len>1 && (ptr[0]!=0 || ptr[1]!=0);
            ptr+=fieldlen, len-=fieldlen)
        {
            if(!IsCompleteBER(ptr, len, &fieldlen))
                return FALSE;
            totlen+=fieldlen;
        }
        if(len>1 && ptr[0]==0 && ptr[1]==0)
        {
            *remainder=headerlen+totlen+2;  /* + 2 nulls at end */
            return TRUE;
        }
        return FALSE;
    }

    if(fieldlen+headerlen<=len)
    {
        *remainder=fieldlen+headerlen;
        return TRUE;
    }
    *remainder=fieldlen+headerlen-len;
    return FALSE;
}

int get_tag(int *tag, CHAR far *s, INT4 len)
{
    CHAR     c;
    int      taglen;

    if(!len)                    /* nothing to look at */
    {
        *tag = -1;
        return 0;
    }

    c=(*s++)&0x1f;              /* the first byte of the tag */
    taglen=1;

    if(c<0x1f)                  /* if the tag is less than 31, then it */
        *tag=c;                 /* is fully contained in the one byte */

    else                        /* otherwise we keep looking at bytes */
    {                           /* until we find 1 with it's sign bit off */
                                /* catenating the last 7 bits of each byte */
        if(len==1)  /* no extra bytes to look at */
        {
            *tag = -1;
            return 0;
        }
        *tag=0;
        c= *s++;
        taglen+=1;
        while(c>0x80 && taglen<len)
        {
            *tag+=c&0x7F;
            *tag<<=7;
            c= *s++;
            taglen+=1;
        }
        if(c>0x80 && taglen==len)  /* missing part of tag */
        {
            *tag = -1;
            return 0;
        }
        *tag+=c;
    }

    return taglen;
}

int get_len(INT4 *fieldlen, CHAR far *s, INT4 len)
{
    int  i;
    CHAR c;
    INT4 tlen;

    if(!len)
    {
        *fieldlen = -1;
        return 0;
    }
    c= *s++;

/* if the sign bit is turned on in the first byte of the length field, then
   the first byte contains the number of subsequent bytes necessary to contain
   the length, or the length is indefinite if the remaining bits are zero.
   Otherwise, the first byte contains the length */

    if(c<128)  /* sign bit off */
    {
        *fieldlen=c;
        return 1;
    }

    tlen=0;
    if( (int)c-128>4)  /* paranoia check: no lengths greater than 2 billion */
    {
        *fieldlen = -1;
        return 0;
    }

    if(c==128)  /* indefinite length */
    {
        *fieldlen = -1;  /* who knows */
        return 1;     /* the length of the length field IS 1 */
    }

    if(c-127>len)  /* not enough length */
    {
        *fieldlen = -1;
        return 0;
    }

    for(i=0; i < (int)c-128; i++)
    {
        tlen<<=8;
        tlen+= *s++;
    }

    *fieldlen=tlen;
#ifdef DEBUG
    printf("in get_len: ASN.1 length field is %d bytes INT4, *len=%ld\n",
        (int)c-127, *fieldlen);
#endif
    return (int)c-127;
}

DATA_DIR far *daddbits(DATA_DIR far *dir, unsigned fldid, CHAR Class,
		    char far *bits)
{
    CHAR     far *data, far *t;
    //DATA_DIR far *new_nodedir;
    int           i, j, len, num_bytes, unused;

    static CHAR mask[8]={0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01};

    len=strlen(bits);
    num_bytes=1+len/8+((len%8)?1:0);
    t=data=dmalloc(dir, num_bytes);
    if(!data)
        return NULL_DIR;

    unused=8-len%8;
    if(unused==8)
        unused=0;

    *t=unused;
    for(i=0, t++; i<len; i+=8, t++)
    {
        *t='\0';
        for(j=0; j<8 && i+j<len; j++, bits++)
            if(*bits=='y' || *bits=='Y' || *bits=='t' || *bits=='T' ||
                *bits=='1')
                t[0]|=mask[j];
    }
    return daddchar(dir, fldid, Class, data, num_bytes);
}

char far *dgetbits(DATA_DIR far *dir)
{
    char *bits;
    int   j, last_used;
    INT4  i;

    static CHAR mask[8]={0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01};

    last_used=8 - dir->ptr.data[0];
    bits=(char far*)dmalloc(dir, 8*(int)dir->count);
    for(i=1; i<dir->count-1; i++)
        for(j=0; j<8; j++)
            bits[(i-1)*8+j]=dir->ptr.data[i]&mask[j]?'y':'n';
    for(j=0; j<last_used; j++)
        bits[(i-1)*8+j]=dir->ptr.data[i]&mask[j]?'y':'n';
    bits[(i-1)*8+j]='\0';
    return bits;
}

CHAR far *dmalloc(DATA_DIR far *dir, int len)
{
    BLK_DIR  far *blk_dir;
    DATA_DIR far *t;

#ifdef DEBUG
    int i;
    INT4 total=0;
#endif

    if(!dir)
        return FALSE;
    blk_dir=dir->block;
    blk_dir->num_blocks++;

#ifdef DEBUG
    printf("in dmalloc1: dir = %lx blk_dir = %lx ", dir, blk_dir);
    printf("block_array = %lx num_blocks = %d\n",
        blk_dir->block_array, (int)blk_dir->num_blocks);
   for(i=0;i<blk_dir->num_blocks-1;i++)
	total += sizeof(*blk_dir->block_array[i]);
   printf("len=%i, total memory allocated = %d\n", len,total);
#endif

    if( (blk_dir->block_array=
        (DATA_DIR far* far*)realloc((CHAR far*)blk_dir->block_array,
	    blk_dir->num_blocks*sizeof(CHAR far*)))==NULL )
    {
#ifdef DEBUG
        printf("in dmalloc: realloc of block_array failed\n");
#endif
        return NULL;
    }

    if( (t = blk_dir->block_array[ blk_dir->num_blocks-1 ] =
        (DATA_DIR far*)malloc(len))==NULL )
	{
#ifdef DEBUG
        printf("in dmalloc: malloc of %d bytes\n", len);
#endif
        return NULL;
	}

#ifdef DEBUG
    printf("in dmalloc2: dir = %lx blk_dir = %lx ", dir, blk_dir);
    printf("block_array = %lx num_blocks = %d\n",
        blk_dir->block_array, (int)blk_dir->num_blocks);
#endif
	return (CHAR far *)t;
}

DATA_DIR far *daddoid(DATA_DIR far *dir, unsigned fldid, CHAR Class,
		    char far *cstring)
{
    CHAR place[100], far *ptr;  /* seems like enough */
    int  offset=0;
    INT4 value;

    while(cstring && isdigit(*cstring))
    {
        if(offset>90)
            return NULL_DIR;
        value = atol(cstring);
        if(offset==0)  /* first 2 numbers get special treatment */
        {
            cstring=strchr(cstring, '.');
            if(cstring)
                cstring++;  /* skip '.' */
            else
                return NULL_DIR;  /* can't be this short */
            value = value * 40 + atol(cstring);
        }

        if (value >= 0x80)
        {
            int  count = 0;
            CHAR bits[12];    /* Save a 84 (12*7) bit number */

            while (value)
            {
                bits[count++] = (CHAR)value & 0x7F;
                value >>= 7;
            }

            /* Now place in the correct order */
            while (--count > 0)
                place[offset++] = bits[count] | 0x80;

            place[offset++] = bits[count];
        }
        else
            place[offset++] = (CHAR)value;

        cstring=strchr(cstring, '.');
        if(cstring)
            cstring++;  /* skip '.' */
    }

    ptr=dmalloc(dir, offset);
    if(!ptr)
        return NULL_DIR;
    COPY(ptr, place, offset);
    return daddchar(dir, fldid, Class, ptr, offset);
}

char far *dgetoid(DATA_DIR far *dir)
{
    char far *s;
    CHAR far *oid;
    int  i, nvals=0, slen;
    INT4 len, offset=0, value[30];  /* 30 numbers should be enough */

    if(!dir || dir->form!=ASN1_PRIMITIVE)
        return NULL;
    oid=dir->ptr.data;
    len=dir->count;
    /* Convert encoded value to a c-string */
    while(offset<len)
    {
        value[nvals] = 0;
        do
        {
            value[nvals] <<= 7;
            value[nvals] |= oid[offset] & 0x7f;
        } while (oid[offset++] & 0x80);

        if(nvals == 0)
            slen=22;
        else
            slen+=11;
        nvals++;
    }
    s=(char far*)dmalloc(dir, slen+1);
    if(!s)
        return NULL;

    sprintf(s, "%ld.%ld", (long)value[0]/40, (long)value[0]%40);
    for(i=1; i<nvals; i++)
      sprintf(s+strlen(s), ".%ld", (long)value[i]);

    return s;
}

DATA_DIR far *dinstag(DATA_DIR far *dir, unsigned fldid, CHAR Class)
{
    DATA_DIR far *new_node;
    //int           count;

    if(dir->block->next_dir > dir->block->last_dir)  /* out of new_node dirs */
        if(!dmake_bigger(dir))
            return FALSE;
    new_node=dir->block->next_dir++;
    if(dir->block->next_dir <= dir->block->last_dir)
        new_node->block->next_dir->block=dir->block;

    if(dir->parent)
    {
        new_node->ptr.child=dir->parent->ptr.child;
        dir->parent->ptr.child=new_node;
        dir->parent->count=1;
    }
    else
        new_node->ptr.child=dir;
    new_node->parent=dir->parent;

    for(new_node->count=0, dir=new_node->ptr.child; dir; 
        new_node->count++, dir=dir->next)
        dir->parent=new_node;

    new_node->next = new_node->prev = NULL_DIR;
    new_node->user=0;

    new_node->fldid=fldid;
    new_node->Class=Class;
    new_node->form =ASN1_CONSTRUCTED;

    return new_node;
}

DATA_DIR *dreplace_num(DATA_DIR *msg, INT4 number)
{
    CHAR     *t;
 
    if(msg->form!=ASN1_PRIMITIVE)
        return(0);
 
    t=(CHAR*)(&msg->number);
if(berutil_ByteOrder == 1)
    msg->number=number;
else {
    CHAR *ptr;
    ptr=(CHAR*)(&number);
    t[0]= ptr[3];
    t[1]= ptr[2];
    t[2]= ptr[1];
    t[3]= ptr[0];
}
/*
#if (NATIVE_ORDER&PDP_ORDER)==PDP_ORDER
    CHAR *ptr;
    ptr=(CHAR*)(&number);
    t[0]= ptr[1];
    t[1]= ptr[0];
    t[2]= ptr[3];
    t[3]= ptr[2];
#endif
*/
    msg->ptr.data=t;
    msg->count = 4;
    if(msg->ptr.data[0]=='\0')
        while(msg->count>1 && msg->ptr.data[0]=='\0' &&
            msg->ptr.data[1]<128)
        {
            msg->ptr.data++;
            msg->count--;
        }
    else
        if(msg->ptr.data[0]==0xff)
            while(msg->count>1 && msg->ptr.data[0]==0xff &&
                msg->ptr.data[1]>127)
            {
                msg->ptr.data++;
                msg->count--;
            }
 
    return(msg);
}
