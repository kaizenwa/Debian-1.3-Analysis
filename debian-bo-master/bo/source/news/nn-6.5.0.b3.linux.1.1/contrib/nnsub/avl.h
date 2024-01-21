/*
    routines for handling AVL tree

    Copyright
	Rudi van Houten, Academic Computer Centre Utrecht
			 Budapestlaan 6  3584 CD  Utrecht  Netherlands
    Author : Rudi van Houten

    The field content is here defined as a character pointer
    but all references to it are made outside these routines
    e.g. the parameter function CNTCMP so the calling program
    is free to use a different definition. In the comments
    this pointertype is called CONTENT.
    The routine CNTCMP is called with two parameters of the
    type CONTENT, and yields an integer value:
        < 0    if 1st par < 2nd par
        = 0    if 1st par = 2nd par
        > 0    if 1st par > 2nd par
    e.g. the routine strcmp.
*/
#include <stdio.h>
extern char * strsave();

typedef struct avl_node
            { char * content;                 /* pointer to data */
              short  balance;                 /* balance the tree */
              struct avl_node *left, *right;  /* tree pointers */
            } AVL_NODE;

typedef AVL_NODE * P_AVL_NODE;

extern int insert_avl();
    /* parameterplan: (value,tree,CNTCMP,rtnval)
                      CONTENT value, *rtnval;
                      P_AVL_NODE *tree;
                      int (*CNTCMP)();
        the variable rtnval yields a reference to the
        datastructure in the found AVL_NODE, it will
        be equal to value if a new node is created.
    */

extern int insavlnode();
    /* parameterplan: (node,p,CNTCMP)
                      P_AVL_NODE node, *p;
                      int (*CNTCMP)();
    */
extern P_AVL_NODE find_avlnode();
    /* parameterplan: (value,tree,CNTCMP)
                      CONTENT value;
                      P_AVL_NODE tree;
                      int (*CNTCMP)();
    */
extern int unravel_avl();
    /* parameterplan: (tree,action,delflag)
                      P_AVL_NODE *tree;
                      int (*action)();
                      int delflag;
    */
