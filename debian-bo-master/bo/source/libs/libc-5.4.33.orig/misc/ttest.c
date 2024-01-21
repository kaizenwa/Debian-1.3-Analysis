#include <stdio.h>
#include <search.h>

struct node {	      /* pointers to these are stored in the tree */
     char *string;
     int count;
};

#define MAXNODES    12
#define MAXSTRING   100
#define MINSTRING   3		/* char, newline, eos */
char string_space[MAXSTRING];	     /* space to store strings */
struct node node_space[MAXNODES];  /* nodes to store */
struct node *root = NULL;	     /* this points to the root */

/*
     This routine compares two nodes, based on an
     alphabetical ordering of the string field.
*/
int node_compare(node1, node2)
     const struct node *node1, *node2;
{
     return strcmp(node1->string, node2->string);
}

/* Print out nodes in alphabetical order */
/*ARGSUSED2*/
void
print_node(node, order, level)
     struct node **node;
     VISIT order;
     int level;
{
     if (order == postorder || order == leaf) {
		    (void) printf("string = %20s,  count = %d\n",
			(*node)->string, (*node)->count);
     }
}

main()
{
     char *strptr = string_space;
     int maxstrlen = MAXSTRING;
     struct node *nodeptr = node_space;
     struct node **found;
     int length;

     while (fgets(strptr, maxstrlen, stdin) != NULL) {
		    /* remove the trailing newline */
		    length = strlen(strptr);
		    strptr[length-1] = 0;
		    /* set node */
		    nodeptr->string = strptr;
		    /* locate node into the tree */
		    found = (struct node **)
			tsearch((void *) nodeptr, (void **) &root, node_compare);
		    /* bump the count */
		    (*found)->count++;
		    if (*found == nodeptr) {
			 /* node was inserted, so get a new one */
			 strptr += length;
			 maxstrlen -= length;
			 if (maxstrlen < MINSTRING)
			      break;
			 if (++nodeptr >= &node_space[MAXNODES])
			      break;
		    }
     }
     twalk((char *)root, print_node);
}
