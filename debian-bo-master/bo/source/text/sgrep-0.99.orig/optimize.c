/*
	System: Structured text retrieval tool sgrep.
	Module: optimize.c 
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Optimizes the operator tree by removing identical
		     subtrees. ( optimize_tree() )
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "defines.h"



int label_c;

/*
 * sorts phrase list, so that same phrases can easily be detected
 */
struct PHRASE_NODE *qsort_phrases(struct PHRASE_NODE **phrase_list)
{
	struct PHRASE_NODE *list1,*list2,*comp,*next,*p_list;
	p_list=*phrase_list;
#ifdef DEBUG
	fprintf(stderr,"\nqsort called ");
#endif
	if (p_list==NULL) 
	{
		return NULL; /* Empty list. Return from recursion */
	}
	comp=p_list;
#ifdef DEBUG
	fprintf(stderr,"comp=%s\n",comp->phrase->s);
#endif
	p_list=p_list->next;
	if (p_list==NULL)
	{
		/* Only one phrase in list. Return from recursion */
		return *phrase_list;
	}
	
	list1=NULL;
	list2=comp;
	comp->next=NULL;
	while(p_list!=NULL)
	{
		next=p_list->next;
		if ( strcmp((char *)comp->phrase->s,
		     (char *)p_list->phrase->s)<0 )
		{
#ifdef DEBUG			
				fprintf(stderr,"list2+%s\n",p_list->phrase->s);
#endif
			p_list->next=list2;
			list2=p_list;
		} else
		{
#ifdef DEBUG
			fprintf(stderr,"list1+%s\n",p_list->phrase->s);
#endif
			p_list->next=list1;
			list1=p_list;
		}
		p_list=next;
	}
	/* order should be now list1 .. comp .. list2 */
	comp=qsort_phrases(&list2);
#ifdef DEBUG
	printf("vika on %s\n",comp->phrase->s);
#endif
	if (list1==NULL) 
	{
		*phrase_list=list2;
		return comp;
	}
	qsort_phrases(&list1)->next=list2;
	*phrase_list=list1;
	return comp;
}

/*
 * Merges duplicate phrases in phrase list
 */
void remove_duplicate_phrases(struct PHRASE_NODE **phrase_list)
{
	struct PHRASE_NODE *pn;
	struct PHRASE_NODE *lpn=NULL;
	struct PHRASE_NODE *tmp;
	char *last;
	
	/* we need to sort phrase list first */
	qsort_phrases(phrase_list);
	pn=*phrase_list;
	
	last=""; /* It's not possible to have empty phrase in the list,
		    so this can never be matched */
	
	while (pn!=NULL)
	{
		if (strcmp(last,(char *)pn->phrase->s)==0)
		{
#ifdef DEBUG
			fprintf(stderr," skipping duplicate\n");
#endif
			/* Phrase was already in the list */
			
			/* We give parent same label the first alike phrase had */
			pn->parent->label_left=label_c;
			/* Removing pn from phrase list */
			lpn->next=pn->next;
			pn->parent->leaf=lpn;
			/* Freeing memory allocated to pn */
			tmp=pn;
			pn=pn->next;
#ifdef ASSERT
			if (pn!=NULL)
			{ 	
				assert(pn->parent!=NULL);
				assert(pn->parent->label_left==LABEL_PHRASE);
			}
#endif
			free(tmp);
			
			/* Statistics... */
			stats.skipped_phrases++;
		} 
		else
		{
			last=(char *)pn->phrase->s;
			label_c++;
			pn->parent->label_left=label_c;
			lpn=pn;
			pn=pn->next;
#ifdef ASSERT
			if (pn!=NULL)
			{ 	
				assert(pn->parent!=NULL);
				assert(pn->parent->label_left==LABEL_PHRASE);
			}
#endif
#ifdef DEBUG
			fprintf(stderr,"Checking duplicates for \"%s\" having label %d\n"
				,last,label_c);
#endif
		}
	}		
}

/* 
 * Recursively adds pointers to parents to every tree and phrase node 
 * counts also operator tree size
 */
void add_parents(struct TREE_NODE *node,struct TREE_NODE *parent)
{
	node->parent=parent;
	
	/* Counting this node to tree_size */
	stats.tree_size++;
	
#ifdef ASSERT
	assert(node->label_right==LABEL_NOTKNOWN);
#endif
	node->refcount=0;
	if (node->oper==PHRASE)
	{
		node->leaf->parent=node;
	} else
	{
#ifdef ASSERT
		assert(node->left!=NULL);
#endif
		add_parents(node->left,node);
		if (node->right!=NULL)
		{
			add_parents(node->right,node);
		}
	}
}

/*
 * Recursively creates a list of leaf nodes from parse tree
 */
int create_leaf_list(struct TREE_NODE *root, struct TREE_NODE **list, int ind)
{
	if (root->oper==PHRASE)
	{
		list[ind]=root;
		return ind+1;
	}
	ind=create_leaf_list(root->left,list,ind);
	if (root->right!=NULL)
	{
		ind=create_leaf_list(root->right,list,ind);
	}
	return ind;
}
	
#ifdef DEBUG
void dump_phrase_list(struct PHRASE_NODE *pn)
{
	while (pn!=NULL)
	{
		fprintf(stderr,"string %s parent label %d having %d references\n",
			pn->phrase->s,pn->parent->label,pn->parent->refcount);
		pn=pn->next;
	}
}
#endif

/*
 * Compares two tree nodes. returns 0 if they are alike
 * alike means: same oper, and same subtrees
 */
int comp_tree_nodes(struct TREE_NODE **n1, struct TREE_NODE **n2)
{
	int x;
	if ( (*n1)->oper==JOIN && (*n2)->oper==JOIN )
	{
		/* Join operation takes int parameter, which much be checked */
		x=(*n1)->number - (*n2)->number;
	} else
	{
		x=(*n1)->oper - (*n2)->oper;
	}
	if (x!=0) return x;
	/* if label_left==LABEL_CONS right subtree must be NULL ! */
#ifdef ASSERT
	assert( (*n1)->label_left!=LABEL_CONS || (*n1)->right==NULL );
	assert( (*n2)->label_left!=LABEL_CONS || (*n2)->right==NULL );
#endif
	if ( (*n1)->label_left==LABEL_CONS && (*n2)->label_left==LABEL_CONS ) 
		return (*n1)!=(*n2);
        x=(*n1)->label_left - (*n2)->label_left;
	if (x!=0) return x;
	x=(*n1)->label_right - (*n2)->label_right;
	return x;
}	

/*
 * sorts a leaf list using stdlib qsort and comp_tree_nodes 
 */
void sort_leaf_list(struct TREE_NODE **leaf_list,int nmemb)
{
#ifdef DEBUG
	fprintf(stderr,"Sorting leaf list of size %d\n",nmemb);
#endif
	qsort(leaf_list,nmemb,sizeof(struct TREE_NODE **),
		(int (*)(const void*,const void*))comp_tree_nodes);
}

/*
 * Removes duplicate subtrees from operator tree
 */
void shrink_tree(struct TREE_NODE *root)
{
	int leaf_list_size;
	int i;
	struct TREE_NODE *dad;
	struct TREE_NODE *me;
	struct TREE_NODE *big_brother;
	int imleft;
	struct TREE_NODE **list0;
	int list0_size;
	struct TREE_NODE **list1;
	int list1_size;
	struct TREE_NODE **tmp;
	
	leaf_list_size=stats.tree_size*sizeof(struct TREE_NODE *);
	list0=(struct TREE_NODE **)e_malloc(leaf_list_size);
	list1=(struct TREE_NODE **)e_malloc(leaf_list_size);
	list0_size=create_leaf_list(root,list0,0);
	list1_size=0;
	
	while (list0_size>1) {
		/* or operators parameters can be swapped */
		for (i=0;i<list0_size;i++)
		{
			if ((list0[i]->oper==OR ||
				list0[i]->oper==EQUAL)
			    && list0[i]->label_left<list0[i]->label_right)
			{
				int tmp;
				struct TREE_NODE *tree_tmp;
#ifdef DEBUG_OPTTREE
				fprintf(stderr,"swapping subtrees\n");
#endif
				tmp=list0[i]->label_left;
				list0[i]->label_left=list0[i]->label_right;
				list0[i]->label_right=tmp;
				tree_tmp=list0[i]->left;
				list0[i]->left=list0[i]->right;
				list0[i]->right=tree_tmp;
			}
		}
		
		sort_leaf_list(list0,list0_size);
#ifdef DEBUG_OPTTREE
		fprintf(stderr,"shrinking tree node list of size %d:\n",list0_size);
#endif
		big_brother=NULL;
		for (i=0;i<list0_size;i++)
		{
			me=list0[i];
			dad=me->parent;
			imleft= (dad->left==me);

			if (big_brother==NULL || comp_tree_nodes(&big_brother,&me)!=0 )
			{
				label_c++;
				big_brother=me;
			} else
			{
				stats.opt_nodes++;
				/* These don't really need to be changed,
				   It just might help catch some bugs */
				me->left=NULL;
				me->right=NULL;
				me->oper=INVALID;
				free(me);
			}
			
			if (imleft)
			{
				dad->label_left=label_c;
				dad->left=big_brother;
			} else
			{
				dad->label_right=label_c;
				dad->right=big_brother;
			}
#ifdef ASSERT
			assert(dad->left!=NULL);
#endif
			if (dad->label_left!=LABEL_NOTKNOWN &&
			     (dad->label_right!=LABEL_NOTKNOWN ||
			      dad->right==NULL) )
			{
				if (dad->right==NULL) dad->label_right=LABEL_NOTKNOWN;
				list1[list1_size++]=dad;
			}				
#ifdef DEBUG_OPTTREE
			fprintf(stderr," label=%-3d oper=%-15s left_label=%-3d right_label=%-3d\n",
				label_c,
				give_oper_name(big_brother->oper),
				big_brother->label_left,
				big_brother->label_right);
#endif
		}
		tmp=list0;
		list0=list1;
		list1=tmp;
		list0_size=list1_size;
		list1_size=0;
	}
}

/*
 * Creates the reference counters
 */
void create_reference_counters(struct TREE_NODE *root)
{

	if (root==NULL) return;	
#ifdef ASSERT
	assert(root->refcount>=0 || root->refcount==-1);
#endif
	if (root->refcount==0)
	{
		root->refcount=1;
		
		/* Lists with these labels should never be freed, because
		 * they are still valid when reusing operator tree
		 * If stream mode is FALSE operator tree will never be
		 * reused
		 */
		if ( !stream_mode &&
		     ( root->label_left==LABEL_START ||
		       root->label_left==LABEL_CONS ||
		       root->label_left==LABEL_CHARS )
		   )
		{
			root->refcount=-1; /* -1 means never free */
		}
		/* end_list constant list must be maintained even when in
		 * stream mode, because it is used in output.c too
		 */
		if ( root->label_left==LABEL_END )
		{
			root->refcount=-1;
		}
		
		create_reference_counters(root->left);
		create_reference_counters(root->right);
	} else
	{
		root->refcount++;
	}
}

#ifdef DEBUG_OPTTREE
/*
 * Prints the optimized tree to stderr
 */
void print_opt_tree(struct TREE_NODE *root, int depth, int label)
{
	int i;
	char line[80];
	static char *visited=NULL;
	
	if (visited==NULL)
	{
		visited=e_malloc(stats.tree_size*2); /* Should be enough */
		for (i=0;i<stats.tree_size*2;i++) visited[i]=FALSE;
	}
	if (depth>50)
	{
		fprintf(stderr,"oops, oper tree depth > 50\n");
		exit(3);
	}
	if (root->refcount>1 && !visited[label])
	{
		sprintf(line," %2d-%3d:",root->refcount,label);
	} else
	{
		if (label==0)
			line[0]=0;
		else
			sprintf(line,"         ");
	}
	for(i=0;i<depth;i++) strcat(line," ");
	if (label!=LABEL_NOTKNOWN)
	{
		if (visited[label])
		{
			fprintf(stderr,"%s^%d\n",line,label);
			return;
		}
		visited[label]=TRUE;
	}
	i=strlen(line);
	if ( root==NULL )
	{
		fprintf(stderr,"\nprint_opt_tree: got NULL node\n");
		exit(3);
	}
	if ( root->oper==PHRASE )
	{
		switch (root->label_left) {
		case LABEL_START:
			sprintf(line+i,"start");
			break;
		case LABEL_END:
			sprintf(line+i,"end");
			break;
		case LABEL_CONS:
			sprintf(line+i,"constant list");
			break;
		case LABEL_CHARS:
			sprintf(line+i,"chars");
			break;
		case LABEL_NOTKNOWN:
			sprintf(line+i,"unknown phrase type");
			break;
		default:
			sprintf(line+i,"\"%s\"",root->leaf->phrase->s);
			break;
		}
		fprintf(stderr,"%s\n",line);
		return;
	}
	if (root->oper<0 || root->oper>R_WORDS)
	{
		printf("\nprint tree: got invalid oper (%d)\n",root->oper);
		exit(3);
	}
	if (root->right!=NULL)
	{
		print_opt_tree(root->left,depth+1,root->label_left);
		sprintf(line+i,"%s",give_oper_name(root->oper));
		fprintf(stderr,"%s\n",line);
		print_opt_tree(root->right,depth+1,root->label_right);
	} else
	{
		sprintf(line+i,"%s(",give_oper_name(root->oper));
		fprintf(stderr,"%s\n",line);
		print_opt_tree(root->left,depth+1,root->label_left);
	}
}
#endif

/*
 * Performs operator tree optimizations
 */
void optimize_tree(struct TREE_NODE **root, struct PHRASE_NODE **phrase_list)
{
	label_c=LABEL_FIRST;
		
	/* We need nodes parent information for optimization */
	add_parents(*root,NULL);
	
#ifdef DEBUG
	fprintf(stderr,"parse tree size is %d\n",stats.tree_size);
#endif
	/* Duplicate phrases are removed and their parents labeled */
	remove_duplicate_phrases(phrase_list);
	
	/* Duplicate subtrees are removed */
	shrink_tree(*root);
	
	create_reference_counters(*root);
#ifdef DEBUG_OPTTREE
	print_opt_tree(*root,0,0);
#endif
}
