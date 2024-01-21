#include <stdio.h>

/*
    Copyright
	Rudi van Houten, Academic Computer Centre Utrecht
			 Budapestlaan 6  3584 CD  Utrecht  Netherlands
    Author : Rudi van Houten

    Purpose:
    C routines for handling AVL tree 
    use the includefiel avl.h in the programs

    The field content is here defined as a character pointer
    but all references to it are made outside these routines
    e.g. the parameter function CNTCMP so the calling program
    is free to use a different definition.
    The routine CNTCMP is called with two parameters of the
    type content, and yields an integer value:
        < 0    if 1st par < 2nd par
        = 0    if 1st par = 2nd par
        > 0    if 1st par > 2nd par
    e.g. the routine strcmp.
--end_doc-
*/

typedef char * CONTENT;
typedef struct avl_node
            { CONTENT content;
              short  balance;                 /* balance the tree */
              struct avl_node *left, *right;  /* tree pointers */
            } AVL_NODE;

typedef AVL_NODE * P_AVL_NODE;

P_AVL_NODE rotate1(p)
P_AVL_NODE *p;
{
    P_AVL_NODE q;
    if ((*p)->balance > 0)
    {
        q= (*p)->right;
        (*p)->right= q->left;
        q->left= *p;
    }
    else /* if ((*p)->balance > 0) */
    {
        q= (*p)->left;
        (*p)->left= q->right;
        q->right= *p;
    } /* if ((*p)->balance > 0) */
    (*p)->balance= 0; q->balance= 0;
    *p= q;
    return(*p);
} /* rotate1 */

AVL_NODE * rotate2(p)
P_AVL_NODE *p;
{
    P_AVL_NODE q, l, r;
    if ( (*p)->balance > 0)
    {
        l= *p; r= (*p)->right; q= r->left;
    }
    else /* if ( (*p)->balance >  0) */
    {
        r= *p; l= (*p)->left; q= l->right;
    } /* if ( (*p)->balance > 0 ) */
    l->balance= r->balance= 0;
    if (q->balance < 0)
        r->balance= 1;
    else
    if (q->balance > 0)
        l->balance= -1;

    q->balance= 0;
    l->right= q->left;
    r->left= q->right;
    q->left= l; q->right= r; *p= q;
    return(*p);
} /* rotate2 */

AVL_NODE * rotate(p)
P_AVL_NODE *p;
{
    if ( (*p)->balance > 0 )
        if ( (*p)->right->balance > 0)
            rotate1(p);
        else
            rotate2(p);
    else
        if ( (*p)->left->balance < 0 )
            rotate1(p);
        else
            rotate2(p);
    return(*p);
} /* rotate */

int insert_avl(value,p,CNTCMP,rtnval)
CONTENT value, *rtnval;
P_AVL_NODE *p;
int (*CNTCMP)();
{
    short k;

    if (*p == NULL)
    {
        *p= (P_AVL_NODE)malloc(sizeof(AVL_NODE));
        (*p)->content= value;
        (*p)->balance= 0;
        (*p)->left= NULL; (*p)->right= NULL;
        *rtnval= value;
        return(1);
    }
    else /* if (*p == NULL) */
    if (k= (*CNTCMP)(value,(*p)->content))
    {
        if (k < 0)
            k= -insert_avl(value,&((*p)->left),CNTCMP,rtnval);
        else
            k= insert_avl(value,&((*p)->right),CNTCMP,rtnval);
        if (k)
        {
            (*p)->balance += k;
            if (abs((*p)->balance) > 1) rotate(p);
            if ((*p)->balance) return(1);
        }
    }
    else *rtnval= (*p)->content;
    return(0);
} /* insert_avl */

int insavlnode(node,p,CNTCMP)
P_AVL_NODE node, *p;
int (*CNTCMP)();
{
    short k;

    if (*p == NULL) *p= node;
    else
    if (k= (*CNTCMP)(node->content,(*p)->content))
    {
        if (k < 0)
            k= -insavlnode(node,&((*p)->left),CNTCMP);
        else
            k= insavlnode(node,&((*p)->right),CNTCMP);
        (*p)->balance += k;
        if (abs((*p)->balance) > 1) rotate(p);

        if ((*p)->balance)
            return(abs(k));
        else
            return(0);
    }
    else return(0);
} /* insavlnode */

P_AVL_NODE find_avlnode(value,tree,CNTCMP)
CONTENT value;
P_AVL_NODE tree;
int (*CNTCMP)(); /* must be the same routine as used by insavlnode */
{
    int k;
    while (1)
    {
        if (tree == NULL) return(NULL);
        if (k= (*CNTCMP)(value,tree->content))
            if (k < 0) tree= tree->left; else tree= tree->right;
        else return(tree);
    }
} /* find_avlnode */

int unravel_avl(tree,action,delflag)
P_AVL_NODE *tree;
int (*action)();
int delflag;
{
    if (*tree == NULL) return(0);
    unravel_avl(&(*tree)->left,action,delflag);
    (*action)((*tree)->content);
    unravel_avl(&(*tree)->right,action,delflag);
    if (delflag) { free(*tree); *tree= NULL; }
    return(1);
} /* unravel_avl */
