/*
	System: Structured text retrieval tool sgrep.
	Module: common.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: common functions used by other modules
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "defines.h"

/* GC-nodes which are freed when evaluating will laid to this list */
struct GC_NODE *free_gc_node=NULL;
/* GC-lists, which are freed when evaluating, will be laid to this list */
struct GC_LIST *free_gc_list=NULL;

/* Pointer to gc_node list index table */
struct GC_NODE **inds;

/*
 * Macro for indexing into gc list. It assumes that there is an index table
 * inds created by function create_node_index 
 */
#define INDEX(IND)	( inds[ (IND)>>GC_NODE_BITS ]-> \
				list[ (IND)& ((1<<GC_NODE_BITS)-1) ] )
				
/*
 * This is malloc which prints error message and exists if memory couldn't
 * be allocated. Keeps track of how much memory has been allocated 
 */
void *e_malloc(size_t size)
{
	void *mem;
	mem=malloc(size);
	if (mem==NULL)
	{
		fprintf(stderr,"Memory allocation failed.\n");
		exit(2);
	}
	stats.e_mallocs+=size;
	return mem;
}

/*
 * This realloc, which prints error message and exits, if memory couldn't
 * be reallocated. Keeps track of how much memory has been allocated
 */
void *e_realloc(void *ptr,size_t size, size_t old_size)
{
	void *mem;
#ifdef DEBUG
	fprintf(stderr,"reallocating %d bytes, old size %d\n",size,old_size);
#endif
	mem=realloc(ptr,size);
	if (mem==NULL)
	{
		fprintf(stderr,"Memory allocation failed.\n");
		exit(2);
	}
	stats.reallocs++;
	stats.e_mallocs+=size-old_size;
	return mem;
}

/*
 * Functions for handling non null terminated strings 
 * non null terminating strings won't work in version 1.0
 */
string *new_string(int size)
{
	string *s;
	
	s=(string *) e_malloc(sizeof(struct STR_STRUCT)+size+1);
#ifdef ASSERT
	s->size=size;
#endif
	s->length=0;
	s->s[0]=0;
	return s;
}

string *init_string(int size, char *src)
{
	string *s;
	
	s=new_string(size);
	memcpy(s->s,src,size);
	s->s[size]=0;
	s->length=size;
	return s;
}

/*
 * Gives new GC_NODE from free_gc_node or e_mallocs one if free_gc_node==NULL 
 */
struct GC_NODE *new_gc_node()
{
	struct GC_NODE *n;
	
	stats.gc_nodes++;
	n=free_gc_node;
	if (n==NULL)
	{
		stats.gc_nodes_allocated++;
		return (struct GC_NODE *) e_malloc(sizeof(struct GC_NODE));
	}
	free_gc_node=free_gc_node->next;
	return n;
}

/*
 * initializes a gc list 
 */
void init_gclist(struct GC_LIST *l)
{
      l->first=new_gc_node();
      l->last=l->first;
      l->last->next=NULL;
      l->last->prev=NULL;
      l->length=0;
      l->nodes=1;
      l->chars=0;
      l->end_sorted=FALSE;
      l->sorted=TRUE;
#ifdef OPTIMIZE_SORTS
      l->nested=FALSE;
#endif
}

/* 
 * Create's and initializes new gc list.
 * Returns pointer to new list
 */
struct GC_LIST *new_gclist()
{
      struct GC_LIST *l;
      
      if (free_gc_list==NULL)
      {
      	l=(struct GC_LIST *) e_malloc(sizeof(struct GC_LIST));
      	stats.gc_lists_allocated++;
      } else
      {
      	l=free_gc_list;
      	free_gc_list=free_gc_list->next;
      }
      init_gclist(l);
      stats.gc_lists++; 
      gc_lists_now++;
      return l;
}
 
/*
 * Turns a gc list to a optimized chars list.
 * in chars list we only tell the length of every region (c->chars)
 * chars list 'contains' every possible region of that size
 * (0,0) (1,1) (2,2) or (1,2) (2,3) (3,4)
 */
void to_chars(struct GC_LIST *c,int chars)
{
#ifdef ASSERT
	assert(c->length==0 && c->last==c->first);
#endif
	c->chars=chars-1;
	if (c->first!=NULL)
	{
		c->first->next=free_gc_node;
		free_gc_node=c->first;
		c->first=NULL;
		c->last=NULL;
	}
	c->length=LAST-chars+2;
	if (c->length<=0)
	{
		/* The gc list became empty, we reinit it to 
		   empty list */
		init_gclist(c);
	}
}

/*
 * Adds a region to gc list 
 * s is start index, e end index
 */
void do_add_region(struct GC_LIST *l, int s, int e)
{
#ifdef DEBUG
	fprintf(stderr,"add region (%d,%d)",s,e);
	if (!l->sorted)
		fprintf(stderr," not sorted");
	if (l->nested)
		fprintf(stderr," nested");
	fprintf(stderr,"\n");
#endif
#ifdef ASSERT
	assert(l->first!=NULL);
	assert(s<=e);
	assert(l->last->next==NULL);
	assert(l->length>=0 || l->length<=GC_NODE_SIZE);
	if (l->length>0 && l->sorted)
	{
		assert(l->last->list[l->length-1].start<=s);
		assert(l->last->list[l->length-1].start!=s ||
			l->last->list[l->length-1].end<e);
	}
#ifdef OPTIMIZE_SORTS
	/* If the region to be added is contained in previous region,
	   there is nesting and l->nested must be TRUE */
	assert(l->length==0 || !l->sorted || l->end_sorted|| l->nested 
	       || e>l->last->list[l->length-1].end );
#endif
	
#endif
 	if (l->length==GC_NODE_SIZE)
 	{
 		l->last->next=new_gc_node();
 		l->last->next->prev=l->last; 
 		l->last=l->last->next;
 		l->last->next=NULL;
 		l->length=0;
 		l->nodes++;
 	}
 	l->last->list[l->length].start=s;
 	l->last->list[l->length].end=e;
 	l->length++;
 	stats.regions++;
}

/*
 * Starts a search for regions in a gc list.
 * Inits GC_POINTER searching handle and returns it
 */
void start_region_search(struct GC_LIST *l, struct GC_POINTER *handle)
{
#ifdef ASSERT
	if (l->last!=NULL)
	{
		assert(l->last->next==NULL);
 		assert(l->length<=GC_NODE_SIZE);
	}
	assert(l->length>=0);
#endif
	handle->list=l;
	handle->ind=0;
	handle->node=l->first;
	stats.scans++;
}

#ifdef LAST_REGION_USED
/*
 * NOT USED (yet)
 * Moves gc lists region pointer to end of list
 *
 */
void last_region(struct GC_LIST *l)
{
#ifdef ASSERT
	if (l->last!=NULL)
	{
		assert(l->last->next==NULL);
 		assert(l->length<=GC_NODE_SIZE);
	}
	assert(l->length>=0);
#endif
	l->current.ind=l->length;
	l->current.node=l->last;
	stats.scans++;
}
#endif

/*
 * Gives next region from gc_list pointed by handle.
 * If all regions have been scanned returns (-1,-1) as region
 */
void do_get_region(struct GC_POINTER *handle, struct REGION *reg)
{
#ifdef ASSERT
	if (handle->list->last!=NULL)
	{
		assert(handle->list->last->next==NULL);
		assert(handle->list->length<=GC_NODE_SIZE);
		assert(handle->node!=NULL);
	 	assert(handle->ind<=GC_NODE_SIZE);
	}
#endif
#ifdef DEBUG
	fprintf(stderr,"do_get_region ");
#endif
	stats.scanned_regions++;
	if ( handle->node==handle->list->last )
	{
		if (handle->ind==handle->list->length)
		{
#ifdef DEBUG
			fprintf(stderr,"list ended\n");
#endif
			reg->start=-1;
			reg->end=-1;
			return;
		}
		if (handle->list->last==NULL)
		{
#ifdef DEBUG
			fprintf(stderr,"chars list\n");
#endif
			/* This is a optimized chars list */
			reg->start=handle->ind;
			reg->end=handle->ind+handle->list->chars;
			handle->ind++;
			return;
		}

	}
	if ( handle->ind==GC_NODE_SIZE )
	{
#ifdef DEBUG
		fprintf(stderr,"node switch ");
#endif
		handle->node=handle->node->next;
		handle->ind=0;
	}
#ifdef DEBUG
	else fprintf(stderr,"normal\n");
#endif
	*reg=handle->node->list[handle->ind++];
}

/*
 * Gives previous region from gc_list pointed by handle.
 * If all regions have been scanned returns (-1,-1) as region
 * NOTE: this function is implemented as a macro in defines.h too, for
 * optimization purposes. However assertions are made only here.
 */
void do_prev_region(struct GC_POINTER *handle,struct REGION *reg)
{
#ifdef DEBUG
	fprintf(stderr,"do_prev_region:");
#endif
#ifdef ASSERT
	if (handle->list->last!=NULL)
	{
		assert(handle->list->last->next==NULL);
		assert(handle->list->length<=GC_NODE_SIZE);
		assert(handle->node!=NULL);
	 	assert(handle->ind<=GC_NODE_SIZE);
	}
	assert(handle->list->length>=0);
	assert(handle->ind>=0);
#endif
	stats.scanned_regions++;
	if ( handle->node==handle->list->first )
	{
		if (handle->ind==0)
		{
#ifdef DEBUG
			fprintf(stderr,"start\n");
#endif
			reg->start=-1;
			reg->end=-1;
			return;
		}
		if (handle->list->first==NULL)
		{
#ifdef DEBUG
			fprintf(stderr,"chars\n");
#endif
			handle->ind--;
			reg->start=handle->ind;
			reg->end=reg->start+handle->list->chars;
			return;
		}
	}
#ifdef ASSERT
	assert(handle->node!=NULL);
#endif
	if ( handle->ind==0 )
	{
#ifdef DEBUG
		fprintf(stderr,"Prev node\n");
#endif
		handle->node=handle->node->prev;
		handle->ind=GC_NODE_SIZE;
	}
#ifdef DEBUG
	else fprintf(stderr,"normal\n");
#endif
	*reg=handle->node->list[--handle->ind];
}

/*
 * Frees a given gc list by putting its GC_NODE's to free_gc_node list 
 * and freeing GC_LIST node-
 */
void free_gclist(struct GC_LIST *l)
{

#ifdef DEBUG
	if (l->first==NULL)
		fprintf(stderr,"Freeing chars list\n");
	else
		fprintf(stderr,"Freeing a list of size %d regions ..",LIST_SIZE(l));
	fflush(stderr);
#endif
	
	if (l->first!=NULL)
	{
		l->last->next=free_gc_node;
		free_gc_node=l->first;
	}
	l->next=free_gc_list;
	free_gc_list=l;
	gc_lists_now--;
#ifdef DEBUG
	fprintf(stderr," done\n");
	fprintf(stderr,"There is %d gc lists now\n",gc_lists_now);
#endif
}

/*
 * creates an index table to nodes of a gc list 
 * index table is needed for referencing regions by their number in gc list.
 */
struct GC_NODE **create_node_index(struct GC_LIST *s)
{
	int i;
	struct GC_NODE **inds;
	
#ifdef DEBUG
	fprintf(stderr,"Creating node index .. ");
	fflush(stderr);
#endif
	inds=(struct GC_NODE **) e_malloc(sizeof(struct GC_NODE *) * s->nodes);
	inds[0]=s->first;
	for(i=1;i<s->nodes;i++)
		inds[i]=inds[i-1]->next;
#ifdef DEBUG
	fprintf(stderr,"Done\n");
#endif
	return inds;
}

/*
 * comparing function for gc_qsort, for sorting by end points 
 */
int end_first(struct REGION creg,int i)
{
	return ( INDEX(i).end < creg.end || 
		   ( INDEX(i).end==creg.end && INDEX(i).start<creg.start ) );
}

/*
 * comparing function for gc_qsort, for sorting by start points 
 */
int start_first(struct REGION creg,int i)
{
	return INDEX(i).start < creg.start || 
		   ( INDEX(i).start==creg.start && INDEX(i).end<creg.end );
}
 
/*
 * Recursive qsort for gc_list. Needs gc node index table created by
 *  create_node_index 
 */
void gc_qsort(int s,int e,
	int (*compar)(struct REGION,int))
{
	struct REGION creg,sreg;
	int i,m,last;	

	if (s>=e) return;
	
	m=(s+e)>>1;
	creg=INDEX(m);
	INDEX(m)=INDEX(s);
	INDEX(s)=creg;
	
	last=s;
	for(i=s+1;i<=e;i++)
	{
		if ( compar(creg,i) )
		{
			last++;
			sreg=INDEX(i);
			INDEX(i)=INDEX(last);
			INDEX(last)=sreg;
		}
	}
	sreg=INDEX(s);
	INDEX(s)=INDEX(last);
	INDEX(last)=sreg;
	gc_qsort(s,last-1,compar);
	gc_qsort(last+1,e,compar);	
}
	
/*
 * Sorts a gc list by its end points 
 */
struct GC_LIST *sort_by_end(struct GC_LIST *s)
{
	int size;
	
	size=LIST_SIZE(s);
	if (size<2) return s;
	
	inds=create_node_index(s);
	
	gc_qsort(0,size-1,end_first);
	free(inds);
	s->end_sorted=TRUE;
	s->sorted=TRUE;
	stats.sorts_by_end++;
	return s;
}

/*
 * Sorts a gc list by its start points 
 */
struct GC_LIST *sort_by_start(struct GC_LIST *s)
{
	int size;
	
	size=LIST_SIZE(s);
	s->end_sorted=FALSE;
	s->sorted=TRUE;
	if (size<2) return s;
	
	inds=create_node_index(s);
	
	gc_qsort(0,size-1,start_first);
	free(inds);
	stats.sorts_by_start++;
	return s;
}

/*
 * Gives user readable operation name from given opernum
 */
const char *give_oper_name(int oper)
{
	switch (oper) {
	case IN:		return "in";break;
	case NOT_IN:		return "not in";break;
	case CONTAINING:	return "containing";break;
	case NOT_CONTAINING:	return "not containing";break;
	case EQUAL:		return "equal";break; /* PK Febr'95 */
	case NOT_EQUAL:		return "not equal";break; /* PK Febr'95 */
	case OR:		return "or";break;
	case ORDERED:		return "..";break;
	case L_ORDERED:		return "_.";break;
	case R_ORDERED:		return "._";break;
	case LR_ORDERED:	return "__";break;
	case EXTRACTING:	return "extracting";break;
	case OUTER:		return "outer";break;
	case INNER:		return "inner";break;
	case CONCAT:		return "concat";break;
	case JOIN:		return "join";break;
	case PHRASE:		return "phrase";break;
	case INVALID:		return "invalid";break;
	default:		return "unknown";break;
	}
}


/*
 * Removes duplica regions from a gc list 
 */
void remove_duplicates(struct GC_LIST *s)
{
	struct GC_POINTER r,s_handle;
	struct GC_NODE *t;
	struct REGION p1,p2;

	stats.remove_duplicates++;
#ifdef ASSERT
	assert((s->sorted) && (!s->end_sorted));
#endif
	start_region_search(s,&r);	
	
	start_region_search(s,&s_handle);	
	get_region(&s_handle,&p1);
	while ( p1.start!=-1 )
	{
		get_region(&s_handle,&p2);
		if ( p1.start!=p2.start || p1.end!=p2.end )
		/* Regions p1 and p2 are different */
		{
			if ( r.ind==GC_NODE_SIZE )
			{
				r.node=r.node->next;
#ifdef ASSERT
				assert(r.node!=NULL);
#endif
				r.ind=0;
			}
#ifdef DEBUG
		fprintf(stderr,"(%d %d)",p1.start,p1.end);
#endif
			r.node->list[r.ind++]=p1;
			p1=p2;
		}
	}
	s->length=r.ind;
	s->last=r.node;
/* Let's free gc blocks which are not needed any more */
	r.node=r.node->next;
	while (r.node!=NULL)
	{
		t=r.node;
		r.node=r.node->next;
		t->next=free_gc_node;
		free_gc_node=t;
	}
	s->last->next=NULL;
}
