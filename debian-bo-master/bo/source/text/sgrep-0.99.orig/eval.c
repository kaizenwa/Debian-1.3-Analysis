/*
	System: Structured text retrieval tool sgrep.
	Module: eval.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Handles the evaluation of sgrep expressions, thus
		     implementing the actual semantics of sgrep language.
		     used through eval() function
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "defines.h"

#if defined(ASSERT) && defined(OPTIMIZE_SORTS)
/* Define this if you want always test that nest optimization works */
#define ASSERT_NESTS
#endif

#ifdef PROGRESS_REPORTS
 /* We have progress reports compiled in */
#define SHOW_PROG do { \
	prog++; \
	if (progress_output && !(prog&1023)) report_progress(oper_name,prog_start,prog); \
} while (0)
#else
 #define SHOW_PROG do {} while (0)
#endif
	
/*
 * Startup size of nest stack 
 */
#define DEFAULT_NEST_DEPTH 1024

/* 
 * Startup size of inner queue 
 */
#define DEFAULT_INNER_QUEUE 1024

struct GC_LIST *real_eval(struct TREE_NODE *root);
struct GC_LIST *or(struct GC_LIST *,struct GC_LIST *);
struct GC_LIST *nest_order(struct GC_LIST *,struct GC_LIST *,int);
struct GC_LIST *quote(struct GC_LIST *,struct GC_LIST *,int);
struct GC_LIST *in(struct GC_LIST *,struct GC_LIST *,int);
struct GC_LIST *containing(struct GC_LIST *,struct GC_LIST *,int);
struct GC_LIST *extracting(struct GC_LIST *,struct GC_LIST *);
struct GC_LIST *outer(struct GC_LIST *);
struct GC_LIST *inner(struct GC_LIST *);
struct GC_LIST *concat(struct GC_LIST *);
struct GC_LIST *join(struct GC_LIST *,int number);
struct GC_LIST *equal(struct GC_LIST *,struct GC_LIST *,int);
int free_tree_node(struct TREE_NODE *node);

/*
 * Recursively evaluates parse tree using operation functions
 * root points the root node of parse tree
 */
struct GC_LIST *eval(struct TREE_NODE *root)
{	
	static int depth=0;
#ifdef DEBUG
	int i;
#endif
	struct GC_LIST *a;
	
	a=root->GC_list;
	depth++;
#ifdef DEBUG
	for(i=0;i<depth;i++) fputc(' ',stderr);
	fprintf(stderr,"Evaluating oper %s l_label=%d r_label=%d\n",
		give_oper_name(root->oper),root->label_left,root->label_right);
#endif
#ifdef ASSERT
	assert(root->oper!=INVALID);
#endif	
	/* If this is a leaf node, we just use leafs gc list */
	if ( a==NULL && root->oper==PHRASE )
	{
#ifdef ASSERT
		assert(root->leaf->GC_list!=NULL);
#endif
		a=root->leaf->GC_list;
		/* OBSOLETE 
			root->leaf->GC_list=NULL;
		*/
		a->refcount=root->refcount;
#ifdef DEBUG
		for(i=0;i<depth;i++) fputc(' ',stderr);
		fprintf(stderr,"Using phrase list %s\n",root->leaf->phrase->s);
#endif
	}
	/* If gc_list is still NULL, it means that it hasn't been evaluated yet */
	if ( a==NULL )
	{
#ifdef DEBUG
		for(i=0;i<depth;i++) fputc(' ',stderr);
		fprintf(stderr,"Calling real eval oper %s\n",
			give_oper_name(root->oper));
#endif
		a=real_eval(root);
		a->refcount=root->refcount;
		/* We free subtrees unneeded gclists */
		if (free_tree_node(root->left))
		{
#ifdef DEBUG
			for(i=0;i<depth;i++) putc(' ',stderr);
			fprintf(stderr,"label %d freed (left)\n",root->label_left);
#endif
		}
		if (free_tree_node(root->right))
		{
#ifdef DEBUG
			for(i=0;i<depth;i++) putc(' ',stderr);
			fprintf(stderr,"label %d freed (right)\n",root->label_right);
#endif
		}
	}
#ifdef DEBUG
	else
	{
		for(i=0;i<depth;i++) fputc(' ',stderr);
		fprintf(stderr,"Using already known list\n");
	}
#endif		
	
	/* Keeps track of longest used gc list */
	if (LIST_SIZE(a)>stats.longest_list)
		stats.longest_list=LIST_SIZE(a);
#ifdef ASSERT_NESTS
	/* We check that if list isn't marked as nested, it really isn't */
	if (!a->nested)
	{
		struct REGION reg1,reg2;
		struct GC_POINTER p;
		
		start_region_search(a,&p);
		get_region(&p,&reg1);
		get_region(&p,&reg2);
		while (reg2.start!=-1)
		{
			assert(reg1.end<reg2.end);
			reg1=reg2;
			get_region(&p,&reg2);
		}
	}
#endif	

#ifdef PROGRESS_REPORTS
	if (progress_output)
	{
		/* After every operation, we clean the line */
		fprintf(stderr,"                                    \r");
	}
#endif
	root->GC_list=a;	

#ifdef DEBUG
	for(i=0;i<depth;i++) fputc(' ',stderr);
	fprintf(stderr,"eval done\n");
#endif
	depth--;
	return a;
}

/*
 * Handles the actual evaluation of some operation
 */
struct GC_LIST *real_eval(struct TREE_NODE *root)
{
	struct GC_LIST *a,*l,*r;

	a=NULL;
#ifdef ASSERT
	assert(root->left!=NULL);
#endif
	
	/* Let's evaluate left and right subtrees first */
	l=eval(root->left);
	/* if root->right==NULL this is a function, and it won't have 
	   right sub tree */
	if (root->right==NULL) r=NULL;
	else r=eval(root->right);
	
	/* Let's call the operation function */
	switch (root->oper) {
	case OR:
		a=or(l,r);
		break;
	case ORDERED:
	case L_ORDERED:
	case R_ORDERED:
	case LR_ORDERED:
		a=nest_order(l,r,root->oper);
		break;
	case QUOTE:
	case L_QUOTE:
	case R_QUOTE:
	case LR_QUOTE:
		a=quote(l,r,root->oper);
		break;
	case IN:
		a=in(l,r,FALSE);
		break;
	case NOT_IN:
		a=in(l,r,TRUE);
		break;
	case CONTAINING:
		a=containing(l,r,FALSE);
		break;
	case NOT_CONTAINING:
		a=containing(l,r,TRUE);
		break;
/* Start PK Febr 95 */
	case EQUAL:
		a=equal(l,r,FALSE);
		break;
	case NOT_EQUAL:
		a=equal(l,r,TRUE);
		break;
/* End PK Febr 95 */
	case OUTER:
		a=outer(l);
		break;
	case INNER:
		a=inner(l);
		break;
	case EXTRACTING:
		a=extracting(l,r);
		break;
	case CONCAT:
		a=concat(l);
		break;
	case JOIN:
		a=join(l,root->number);
		break;
	default:
		fprintf(stderr,"Unsupported operation in parse tree (%d)\n",
			root->oper);
		exit(3);
		break;
	}
	
	return a;
}

/*
 * Decrements tree nodes reference counter, and frees nodes gc list if
 * counter comes down to 0. Returns 1 if something was freed, 0
 * otherwise
 */
int free_tree_node(struct TREE_NODE *node)
{
	if (node==NULL) return 0; /* This was a leaf or function node */ 
	if (node->GC_list!=NULL) 
	{
		if (node->GC_list->refcount!=-1)
			node->GC_list->refcount--;
#ifdef ASSERT
		assert(node->GC_list->refcount>=0 
			|| node->GC_list->refcount==-1);
#endif
		if (node->GC_list->refcount==0)
		{
			free_gclist(node->GC_list);
			node->GC_list=NULL;
			return 1;
		}
	}
	return 0;
}

#ifdef PROGRESS_REPORTS
/*
 * Shows a progress report on stderr
 */
void report_progress(char *line,int size, int now)
{
	fprintf(stderr,"%s %d%% done%s\r",
		line,(100*now)/size,"                  ");
	fflush(stderr);
}
#endif

/*
 * Gives first region from two gc_lists, eliminating same regions.
 */
struct REGION first_of(struct GC_POINTER *lp,struct GC_POINTER *rp)
{
	struct REGION l_reg,r_reg;
	
	/* quite straightforward limiting of two gc lists.
	   same regions are concatanated */	
	get_region(lp,&l_reg);
	get_region(rp,&r_reg);
	if (r_reg.start!=-1 && l_reg.start!=-1)
	{
		if (l_reg.start<r_reg.start)
		{
			prev_region(rp,&r_reg);
			return l_reg;
		} else if (l_reg.start>r_reg.start)
		{
			prev_region(lp,&l_reg);
			return r_reg;
		} else if (l_reg.end<r_reg.end)
		{
			prev_region(rp,&r_reg);
			return l_reg;
		} else if (l_reg.end>r_reg.end)
		{
			prev_region(lp,&l_reg);
			return r_reg;
		} else 
		{
			return r_reg;
		}
	}
	if (r_reg.start!=-1) return r_reg;
	if (l_reg.start!=-1) return l_reg;
	/* Both lists were empty, we return (-1,-1) */
	return r_reg;
}

/*
 * Handles or operation
 */
struct GC_LIST *or(struct GC_LIST *l,struct GC_LIST *r)
{
	struct GC_POINTER lp,rp;
	struct GC_LIST *a;
	struct REGION tmp;
#ifdef OPTIMIZE_SORTS
	struct REGION prev;
#endif
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"or called\n");
#endif
	stats.or++;
	a=new_gclist();
#ifdef OPTIMIZE_SORTS
	prev.start=-1;
	prev.end=-1;
#endif
	start_region_search(l,&lp);
	start_region_search(r,&rp);

#ifdef PROGRESS_REPORTS
	prog_start=LIST_SIZE(r)+LIST_SIZE(l);
	prog=0;
	oper_name="or";
#endif

	for(tmp=first_of(&lp,&rp);tmp.start!=-1;tmp=first_of(&lp,&rp))
	{
#ifdef OPTIMIZE_SORTS
		if ( tmp.end<=prev.end )
		{
			/* We had nesting */
			a->nested=TRUE;
		}
#endif
		SHOW_PROG;
		add_region(a,tmp.start,tmp.end);
#ifdef OPTIMIZE_SORTS
		prev=tmp;
#endif
	}
	return a;
}
	
/* 
 * Handles ordering which produces possibly nesting gc-lists. 
 */
struct GC_LIST *nest_order(struct GC_LIST *l,struct GC_LIST *r,int type)
{
	struct GC_POINTER lp,rp;
	struct GC_LIST *a;
	struct REGION r_reg,l_reg;              
	static struct REGION *nest_stack=NULL;
	int nest_depth=0;
	int nestings;
	int s,e;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"nest_order called\n");
#endif
	start_region_search(r,&rp);
	
	if (nest_stack==NULL)
	{
		stats.nest_stacksize=DEFAULT_NEST_DEPTH*sizeof(*nest_stack);
		nest_stack=(struct REGION *)e_malloc(stats.nest_stacksize);
	}
	stats.order++;
	a=new_gclist();
	a->sorted=FALSE;
	a->nested=l->nested || r->nested;
#ifdef DEBUG
	if (a->nested) fprintf(stderr,"inherited nesting\n");
#endif

#ifdef OPTIMIZE_SORTS
	if (l->nested) sort_by_end(l);
	else stats.sorts_optimized++;
#else
	sort_by_end(l);
#endif

#ifdef PROGRESS_REPORTS
	prog_start=LIST_SIZE(r);
	prog=0;
	switch (type) {
	case L_ORDERED:
		oper_name="_.";
		break;
	case R_ORDERED:
		oper_name="._";
		break;
	case LR_ORDERED:
		oper_name="__";
		break;
	default:
		oper_name="..";
	}
#endif
	nestings=0;
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);
	get_region(&rp,&r_reg);
	SHOW_PROG;
	/* If left or right region list was empty, we can return empty list */
	if (l_reg.start==-1 || r_reg.start==-1) return a;
	
	do
	{
		if (l_reg.end<r_reg.start && l_reg.start!=-1 )
		{
			/* left region is first. Add to nest_stack
			   and nest queue */
			if (nest_depth*sizeof(*nest_stack)==stats.nest_stacksize)
			{
				nest_stack=(struct REGION *)
					e_realloc(nest_stack,
						stats.nest_stacksize*2,
						stats.nest_stacksize);
				stats.nest_stacksize*=2;

			}
			nest_stack[nest_depth++]=l_reg;
			nestings=0;
#ifdef DEBUG
			if (nest_depth==1)
				fprintf(stderr," New q");
			else fprintf(stderr," +");
			fprintf(stderr,"(%d:%d)",l_reg.start,l_reg.end);
#endif
			get_region(&lp,&l_reg);
		}
		else if (nest_depth>0)
		{
#ifdef DEBUG
			fprintf(stderr," %d",r_reg.end);
#endif
			if (type==L_ORDERED || type==LR_ORDERED)
				s=nest_stack[--nest_depth].end+1;
			else s=nest_stack[--nest_depth].start;
			if (type==R_ORDERED || type==LR_ORDERED)
				e=r_reg.start-1;
			else e=r_reg.end;

			if (e>=s)
			{
				add_region(a,s,e);
#ifdef OPTIMIZE_SORTS
				/* If we have taken region from nest stack
				   twice in row, it probably means, that
				   we have a nested result list */
				nestings++;
				if (nestings==2)
				{
#ifdef DEBUG
					if (!a->nested)
					fprintf(stderr,"nesting order detecded\n");
#endif
					a->nested=TRUE;
				}
#endif
			}			
			get_region(&rp,&r_reg);
			SHOW_PROG;
		} else 
		{
			get_region(&rp,&r_reg);
			SHOW_PROG;
		}
	} while ( r_reg.start!=-1 );
#ifdef OPTIMIZE_SORTS
	if (!a->nested) stats.sorts_optimized++;
	a->sorted=!a->nested;
#endif
	if (!a->sorted) sort_by_start(a);
	return a;	
}

/*
 * Handles in operation 
 */
struct GC_LIST *in(struct GC_LIST *l,struct GC_LIST *r, int not)
/* Changed by PK in Febr 95 to capture the semantics of _proper_
containment */
{	
	struct GC_POINTER lp,rp;
	struct GC_LIST *a,*r2;
	struct REGION r_reg,l_reg,r_reg2;
	char *oper_name;
#ifdef PROGRESS_REPORTS
	int prog;int prog_start;
#endif
			
#ifdef DEBUG
	fprintf(stderr,"in called\n");
#endif
	if (not) 
	{
		stats.not_in++;
		oper_name="not in";
	} else 
	{
		stats.in++;
		oper_name="in";
	}
	a=new_gclist();
	
#ifdef OPTIMIZE_SORTS
	a->nested=l->nested;
#endif
	
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);
	
	
	/* 
 	* To simplify things we do an outer function on right gc_list 
 	*/
#ifdef OPTIMIZE_SORTS
	if (r->nested)
	{
#endif
		r2=outer(r);
		r=r2;
#ifdef OPTIMIZE_SORTS
	} else r2=NULL;
#endif
	start_region_search(r,&rp);

#ifdef PROGRESS_REPORTS
	prog_start=LIST_SIZE(l)+LIST_SIZE(r);
	prog=0;
#endif

	get_region(&rp,&r_reg);
	SHOW_PROG;
	while (r_reg.start!=-1 && l_reg.start!=-1)
	{
#ifdef DEBUG
		fprintf(stderr,"in: left=(%d,%d) right=(%d,%d)\n",
			l_reg.start,l_reg.end,
			r_reg.start,r_reg.end);
#endif
		if (l_reg.start<r_reg.start)
		{
			/* Left region starts before right -> can't be
			   in right region or any right region that follows
			   current one */
			if (not) add_region(a,l_reg.start,l_reg.end);
			get_region(&lp,&l_reg);
			SHOW_PROG;
		} else /* l_reg.start>=r_reg.start */
		{
			if (l_reg.end<=r_reg.end)
			{
				/* left region is in right region */
/* Start PK Febr 95 */
				if (l_reg.start>r_reg.start ||
				    l_reg.end<r_reg.end)
				{	/* inclusion is proper */
					if (!not) add_region(a,l_reg.start,l_reg.end);
					get_region(&lp,&l_reg);
					SHOW_PROG;
				} else { /* l_reg == r_reg */ 
					if (not) add_region(a,l_reg.start,l_reg.end);
					get_region(&lp,&l_reg);
					SHOW_PROG;
				}
/* End PK Febr 95 */
			} else 	if (l_reg.start==r_reg.start)
			{
				/* Regions start from same place. Because
				no right region after current one can start 
				from same place we can skip left region */
				if (not) add_region(a,l_reg.start,l_reg.end);
				get_region(&lp,&l_reg);
				SHOW_PROG;
			} else
			{
				/* left and right region are overlapping */
#ifdef DEBUG
				fprintf(stderr,"in overlap\n");
#endif
				get_region(&rp,&r_reg2);
				if (r_reg2.start==-1)
				{
					/* All right regions have been scanned */
					if ( l_reg.start > r_reg.end )
					{
						/* Left region end after last right region. 
						   We can fall out of loop */
						SHOW_PROG;
						r_reg=r_reg2;
					} else
					{
						/* Next left region might still be in right 
						   region */
						if (not) add_region(a,l_reg.start, l_reg.end);
						get_region(&lp,&l_reg);
						SHOW_PROG;
					}
				} else
				{
					/* There are still right regions */
					if ( l_reg.start >= r_reg2.start )
					{
						/* Since left region starts after new right region,
						 * We can safely skip previous right */
						r_reg=r_reg2;
						SHOW_PROG;
					} else
					{
						/* Left region is not in previous or next 
						   right region */
						prev_region(&rp,&r_reg2);
						if (not) add_region(a,l_reg.start, l_reg.end);
						get_region(&lp,&l_reg);
						SHOW_PROG;
					}
				}
			}
		}
	}

#ifdef DEBUG
	fprintf(stderr,"in fall out\n");
#endif
/* If we have "not in" and right gc_list is empty, we need to copy
   rest of left list */
	if (not)
	{
		while (l_reg.start!=-1)
		{
			add_region(a,l_reg.start,l_reg.end);
			get_region(&lp,&l_reg);
			SHOW_PROG;
		}
	}

/* because we created list r2 here, we free it here */
	if (r2!=NULL) free_gclist(r2);
	return a;
}

/* 
 * Handles outer function 
 */
struct GC_LIST *outer(struct GC_LIST *gcl)
{
	struct GC_POINTER p;
	struct REGION reg1,reg2;
	struct GC_LIST *a;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef PROGRESS_REPORTS
	oper_name="outer";
	prog_start=LIST_SIZE(gcl);
	prog=0;
#endif

	stats.outer++;
	reg2.start=0;
	a=new_gclist();
	start_region_search(gcl,&p);
	get_region(&p,&reg1);

	/* if we had empty gc list */
	if (reg1.start==-1) return a;
	SHOW_PROG;
		
	/* If there are many regions starting from same place, we choose the
	   longest */
	get_region(&p,&reg2);
	SHOW_PROG;
	while (reg2.start==reg1.start && reg2.end>reg1.end)
	{
		reg1=reg2;
		get_region(&p,&reg2);
		SHOW_PROG;
	}
	
	while(reg1.start!=-1 && reg2.start!=-1)
	{
		if (reg2.end>reg1.end && reg2.start!=reg1.start)
		{
			/* reg2 ends after reg1 -> no nesting */
			add_region(a,reg1.start,reg1.end);
			reg1=reg2;
		}
		get_region(&p,&reg2);
		SHOW_PROG;
		/* If regions start from same place, nesting is guaranteed */
		if (reg2.start==reg1.start)
		{
			reg1=reg2;
			get_region(&p,&reg2);
			SHOW_PROG;
		}
	}
	add_region(a,reg1.start,reg1.end);
	return a;
}

/*
 * Handles inner function 
 */
struct GC_LIST *inner(struct GC_LIST *gcl)
{
	static struct REGION *inner_stack;
	struct GC_POINTER p;
	int inq_ind=0;
	struct GC_LIST *a=NULL;
	struct REGION n_reg,c_reg;
	int i;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"inner called\n");
#endif	
	stats.inner++;
	a=new_gclist();
	if (stats.inner_tablesize==0)
	{
		stats.inner_tablesize=
			DEFAULT_INNER_QUEUE*sizeof(*inner_stack);
		inner_stack=(struct REGION *) e_malloc(stats.inner_tablesize);
	}

#ifdef PROGRESS_REPORTS
	prog_start=LIST_SIZE(gcl);
	prog=0;
	oper_name="inner";
#endif

	start_region_search(gcl,&p);
	get_region(&p,&c_reg);
	SHOW_PROG;
	while (c_reg.start!=-1) {
		get_region(&p,&n_reg);
		SHOW_PROG;
#ifdef ASSERT
		assert(n_reg.start>=c_reg.start || n_reg.start==-1 );
#endif
		if ( n_reg.start>c_reg.end || n_reg.start==-1 )
		{
			/* n_reg and c_reg are separate. Therefore c_reg must
			   be innermost */
			/* Now we can empty inner_stack */
#ifdef DEBUG
			fprintf(stderr,"empty inner stack (%d regions)\n",inq_ind);
#endif
			for (i=0;i<inq_ind;i++)
			{
#ifdef ASSERT
				assert(inner_stack[i].start<=c_reg.start);
#endif
				if (inner_stack[i].end<c_reg.end)
				/* Region in inner_stack was innermost */
					add_region(a,inner_stack[i].start,
						inner_stack[i].end);
			}
			inq_ind=0;
			add_region(a,c_reg.start,c_reg.end);
		} else if ( n_reg.end>c_reg.end )
		{
			/* n_reg and c_reg are overlapping. Let's add c_reg
			   to inner_stack */
			if (inq_ind*sizeof(*inner_stack)==stats.inner_tablesize)
			{
				inner_stack=(struct REGION *) e_realloc(inner_stack,
					stats.inner_tablesize*2,stats.inner_tablesize);
				stats.inner_tablesize*=2;
			}
			inner_stack[inq_ind++]=c_reg;
		} else {
		/* if neither of the previous if's was taken, 
		   c_reg contains n_reg. We remove regions containing n_reg from
		   inner_stack */
			while(inq_ind &&  
				n_reg.start>=inner_stack[inq_ind-1].start &&
				n_reg.end<=inner_stack[inq_ind-1].end )
			{
				inq_ind--;
			}
		}
		c_reg=n_reg;
#ifdef ASSERT
		if (inq_ind) 
			assert(c_reg.start<inner_stack[inq_ind-1].start ||
				c_reg.end>inner_stack[inq_ind-1].end);
#endif
	}
	return a;
}

struct GC_LIST *containing(struct GC_LIST *l,struct GC_LIST *r,int not)
/* Changed by PK in Febr 95 to capture the semantics of _proper_
containment */
{
	struct GC_POINTER lp,rp;
	struct GC_LIST *a,*r2;
	struct REGION r_reg,l_reg;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"containing called\n");
#endif
	if (not) stats.not_containing++; else stats.containing++;
	a=new_gclist();
#ifdef OPTIMIZE_SORTS
	a->nested=l->nested;
#endif
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);

/* To simplify things we do an inner function on right gc_list */
#ifdef OPTIMIZE_SORTS
	if (r->nested)
	{
#endif
		r2=inner(r);
		r=r2;
#ifdef OPTIMIZE_SORTS
	} else r2=NULL;
#endif

#ifdef PROGRESS_REPORTS
	oper_name= (not) ? "not containing" : "containing";
	prog=0;
	prog_start=LIST_SIZE(l)+LIST_SIZE(r);
#endif
	start_region_search(r,&rp);
	
	get_region(&rp,&r_reg);
	SHOW_PROG;
	while (r_reg.start!=-1 && l_reg.start!=-1)
	{
		if ( l_reg.start>r_reg.start )
		{
			/* right starts before left */
			get_region(&rp,&r_reg);
			SHOW_PROG;
		} else if ( l_reg.end>=r_reg.end )
		{
			/* left contains right */
/* Start PK Febr 95 */
			if (l_reg.start<r_reg.start ||
			    l_reg.end>r_reg.end)
			{	/* Containment is proper */
				if (!not) add_region(a,l_reg.start,l_reg.end);
				get_region(&lp,&l_reg);
				SHOW_PROG;
			} else { /* l_reg == r_reg */
				if (not) add_region(a,l_reg.start,l_reg.end);
				get_region(&lp,&l_reg);
				SHOW_PROG;
			}
/* End PK Febr 95 */
		} else {
			/* left comes after right */
			if (not) add_region(a,l_reg.start,l_reg.end);
			get_region(&lp,&l_reg);
			SHOW_PROG;
		}	
	}
	/* When right list ended, there still might be something in left list */
	while (not && l_reg.start!=-1)
	{
		add_region(a,l_reg.start,l_reg.end);
		get_region(&lp,&l_reg);
		SHOW_PROG;
	}
/* because we created list r2 here, we free it here */
	if (r2!=NULL) free_gclist(r2);
	return a;
}

struct GC_LIST *equal(struct GC_LIST *l,struct GC_LIST *r,int not)
/* Intersection of GC_LISTs *l and *r */
/* PK Febr '95 */
{
	struct GC_POINTER lp,rp;
	struct GC_LIST *a;
	struct REGION r_reg,l_reg;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"equal called\n");
#endif
	if (not) stats.not_equal++; else stats.equal++;
	a=new_gclist();
#ifdef OPTIMIZE_SORTS
	a->nested=l->nested;
#endif
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);


#ifdef PROGRESS_REPORTS
	oper_name= (not) ? "not equal" : "equal";
	prog=0;
	prog_start=LIST_SIZE(l)+LIST_SIZE(r);
#endif
	start_region_search(r,&rp);
	get_region(&rp,&r_reg);

	SHOW_PROG;

	while (r_reg.start!=-1 && l_reg.start!=-1)
	{
		if ( l_reg.start<r_reg.start )
		{
			if (not) add_region(a,l_reg.start,l_reg.end);
			get_region(&lp,&l_reg);
			SHOW_PROG;
		} else if ( r_reg.start<l_reg.start )
		{
			get_region(&rp,&r_reg);
			SHOW_PROG;
		} else  /*  r_reg.start=l_reg.start */
			if ( l_reg.end<r_reg.end )
			{
				if (not) add_region(a,l_reg.start,l_reg.end);
				get_region(&lp,&l_reg);
				SHOW_PROG;
			} else if ( r_reg.end<l_reg.end )
			{
				get_region(&rp,&r_reg);
				SHOW_PROG;
			} else /* l_reg = r_reg */
			{
				if (!not) add_region(a,l_reg.start,l_reg.end);
				get_region(&rp,&r_reg);
				get_region(&lp,&l_reg);
				SHOW_PROG;
			}
	}
	/* When right list ended, there still might be something in left list */
	while (not && l_reg.start!=-1)
	{
		add_region(a,l_reg.start,l_reg.end);
		get_region(&lp,&l_reg);
		SHOW_PROG;
	}
	return a;

} /* END equal(struct GC_LIST *l,struct GC_LIST *r,int not) */

/*
 * Here we implement concat operation, which concats all overlapping regions
 * into one region and removes all nestings
 */
struct GC_LIST *concat(struct GC_LIST *l)
{
	struct GC_POINTER lp;
	struct GC_LIST *a;
	struct REGION reg1,reg2;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"concat called\n");
#endif
	stats.concat++;
	a=new_gclist();
	start_region_search(l,&lp);
	get_region(&lp,&reg1);
	
	/* We had empty list */
	if (reg1.start==-1) return a;

#ifdef PROGRESS_REPORTS
	oper_name="concat";
	prog=0;
	prog_start=LIST_SIZE(l);
#endif
	SHOW_PROG;	
	get_region(&lp,&reg2);
	SHOW_PROG;
	
	while (reg2.start!=-1)
	{
		if (reg2.start>reg1.end+1)
		{
			/* separate regions, no concat */
			add_region(a,reg1.start,reg1.end);
			reg1=reg2;
		} else if ( reg2.end>reg1.end )
		{
			/* We found overlapping */
			reg1.end=reg2.end;
		}
		get_region(&lp,&reg2);
		SHOW_PROG;
	}
	add_region(a,reg1.start,reg1.end);
	return a;
}

/*
 * Here we implement extracting operation
 */
struct GC_LIST *extracting(struct GC_LIST *l,struct GC_LIST *r)
{
	struct GC_POINTER lp,rp,tmpp;
	struct GC_LIST *a,*r2,*tmp,*new_tmp;
	struct REGION l_reg,r_reg;
	int prev_s=-1;
	int prev_e=-1;
	int last_tmp;
	int must_be_sorted;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"extracting called\n");
#endif
	stats.extracting++;
	/* to simplify things we do concat on right gc_list. Result stays
	   the same anyway */
	r2=concat(r);
	r=r2;
	
	a=new_gclist();
#ifdef OPTIMIZE_SORTS
	a->nested=l->nested;
#endif
	tmp=new_gclist();
	start_region_search(tmp,&tmpp);

#ifdef PROGRESS_REPORTS
	oper_name="extracting";
	prog=0;
	prog_start=LIST_SIZE(l);
#endif
	
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);
	SHOW_PROG;
	start_region_search(r,&rp);
	get_region(&rp,&r_reg);

	while ( l_reg.start!=-1 )
	{
		if ( l_reg.end<r_reg.start || r_reg.start==-1 )
		{
			/* Regions are separate, left starting first.
 			   no cutting */
 			if ( prev_s!=l_reg.start || prev_e!=l_reg.end )
 			{				
#ifdef DEBUG		
	fprintf(stderr,"extracting adding 1(%d,%d)\n",l_reg.start,l_reg.end);
#endif
				prev_s=l_reg.start;
				prev_e=l_reg.end;
				add_region(a,l_reg.start,l_reg.end);
			}
			l_reg=first_of(&lp,&tmpp);
			SHOW_PROG;
		} else if ( r_reg.end<l_reg.start )
		{
			/* Regions are separate right starting first.
			   we skip right */
#ifdef DEBUG
			fprintf(stderr,"skipping right, left=(%d,%d) right=(%d,%d)\n",
				l_reg.start,l_reg.end,r_reg.start,r_reg.end);
#endif
			get_region(&rp,&r_reg);
		} else
		{
			/* We need to do clipping. */
			new_tmp=new_gclist();
			new_tmp->sorted=FALSE;
			must_be_sorted=FALSE;
			last_tmp=-1;
#ifdef DEBUG
			fprintf(stderr,"cutting loop, cutter (%d,%d)\n",r_reg.start,r_reg.end);
#endif
			while ( l_reg.start!=-1 && l_reg.start<=r_reg.end )
			{
				if (l_reg.start<r_reg.start
			&& ( prev_s!=l_reg.start ||
			prev_e!=r_reg.start-1 ) 
					)
				{
					prev_s=l_reg.start;
					prev_e=r_reg.start-1;

#ifdef DEBUG
	fprintf(stderr,"extracting adding 2(%d,%d)\n",l_reg.start,r_reg.start-1);
#endif		
					add_region(a,l_reg.start,r_reg.start-1);
				}
				if (r_reg.end<l_reg.end)
				{
#ifdef DEBUG
	fprintf(stderr,"(%d,%d)<-new_tmp\n",r_reg.end+1,l_reg.end);
#endif
					add_region(new_tmp,
						r_reg.end+1,l_reg.end);
					if (l_reg.end<last_tmp)
					{
						must_be_sorted=TRUE;
					} else last_tmp=l_reg.end;
				}
				l_reg=first_of(&lp,&tmpp);
				SHOW_PROG;
			}
			if (l_reg.start!=-1) prev_region(&lp,&l_reg);
				
#ifdef ASSERT
			assert (tmpp.ind==tmp->length &&
				tmpp.node->next==NULL);
#endif
			free_gclist(tmp);
			if (must_be_sorted)
				sort_by_start(new_tmp);
#ifdef PROGRESS_REPORTS
			prog--;
			prog_start+=LIST_SIZE(new_tmp);
#endif
			tmp=new_tmp;
			start_region_search(tmp,&tmpp);
			/* Left region is now handled -> skip to next */
			l_reg=first_of(&lp,&tmpp);
			SHOW_PROG;
		}
	}
	free_gclist(r2);
	free_gclist(tmp);
	return a;
}

/*
 * Join operation
 */
struct GC_LIST *join(struct GC_LIST *l,int number)
{
	struct GC_LIST *a;
	struct GC_POINTER p1,p2;
	struct REGION r1,r2,prev_r1,prev_r2;
	int i;
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif
	
#ifdef DEBUG
	fprintf(stderr,"join called %d\n",number);
#endif
#ifdef ASSERT
	assert(number>0);
#endif
	stats.join++;
	a=new_gclist();
#ifdef OPTIMIZE_SORTS
	a->nested=l->nested;
#endif	
	if ( l->first==NULL )
	{
		/* This is an optimized chars node */
		to_chars(a,(l->chars+1)*number);
		return a;
	}
	
	/* List is smaller than join number, so return list is empty */
	if (LIST_SIZE(l)<number) return a;

#ifdef PROGRESS_REPORTS
	oper_name="join";
	prog=0;
	prog_start=LIST_SIZE(l);
#endif

	start_region_search(l,&p1);
	start_region_search(l,&p2);

	prev_r2.start=-1;	
	prev_r1.end=-1;	
	for (i=number;i>0;i--)
	{
		get_region(&p1,&r1);
		SHOW_PROG;
#ifdef ASSERT
		assert(r1.start!=-1);
#endif
	}
	while (r1.start!=-1)
	{
		get_region(&p2,&r2);
		if (r2.start==prev_r2.start)
		{
/*PK			if (prev.end<r2.end) */

			if (r1.end<=prev_r1.end)
			{
				a->sorted=FALSE;
			}
		}
		add_region(a,r2.start,r1.end);
		prev_r1=r1; 
		get_region(&p1,&r1);
		SHOW_PROG;
		prev_r2=r2; 
	}
	if (!a->sorted)
	{ 
		sort_by_start(a); /* This may leave duplicates in a */
	  	remove_duplicates(a); /* Here they are removed */
	};
	return a;	
}

/* 
 * Handles ordering which does _not_ produce nesting gc-lists. 
 * For example '"--" quote "--"' to catch SGML comments.
 */
struct GC_LIST *quote(struct GC_LIST *l,struct GC_LIST *r,int type)
{
	struct GC_POINTER lp,rp;
	struct GC_LIST *a;
	struct REGION r_reg,l_reg;              
#ifdef PROGRESS_REPORTS
	char *oper_name;
	int prog;int prog_start;
#endif

#ifdef DEBUG
	fprintf(stderr,"quote called\n");
#endif	
	stats.quote++;
	a=new_gclist();
	a->sorted=TRUE;
	a->nested=FALSE;

#ifdef PROGRESS_REPORTS
	prog_start=LIST_SIZE(r);
	prog=0;
	switch (type) {
	case L_QUOTE:
		oper_name="_quote";
		break;
	case R_QUOTE:
		oper_name="quote_";
		break;
	case LR_QUOTE:
		oper_name="_quote_";
		break;
	default:
		oper_name="quote";
	}
#endif
	start_region_search(r,&rp);
	start_region_search(l,&lp);
	get_region(&lp,&l_reg);
	get_region(&rp,&r_reg);
	SHOW_PROG;

	/* If left or right region list was empty, we can return empty list */
	if (l_reg.start==-1 || r_reg.start==-1) return a;
	do {
		/* Skip until we find ending quote after start quote */
		while (l_reg.end>=r_reg.start && r_reg.start!=-1) {
			get_region(&rp,&r_reg);
			SHOW_PROG;
		}
		if (r_reg.start>=0) {
			/* Add region using operation type */
			switch (type) {
			case QUOTE:
				add_region(a,l_reg.start, r_reg.end);
				break;
			case L_QUOTE:
				add_region(a,l_reg.end+1, r_reg.end);
				break;
			case R_QUOTE:
				add_region(a,l_reg.start, r_reg.start-1);
				break;
			case LR_QUOTE:
				/* No empty regions */
				if (l_reg.end+1<r_reg.start) {
					add_region(a,l_reg.end+1,r_reg.start-1);
				} 
				break;
			default:
				fprintf(stderr,"quote: invalid oper type\n");
				exit(2);			
			/* Skip until starting quote is after last ending
			   quote */
			}
			while (l_reg.start<=r_reg.end && l_reg.start!=-1) {
				get_region(&lp,&l_reg);
				SHOW_PROG;
			}
		}
	} while ( l_reg.start!=-1 && r_reg.start!=-1);
	return a;
}

