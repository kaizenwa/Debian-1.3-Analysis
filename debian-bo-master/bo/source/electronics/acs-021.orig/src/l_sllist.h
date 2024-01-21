/*
  Philip White, mod by Al Davis
  list.h
  class example
  This is the header for the list class.  The list class is a singularly
  linked list with a dummy head and tail node.  The nodes of this list
  are of type "node" which is a struct that contains an element of type
  "list_element" (which in this example is an integer) and a next
  pointer which is used to link the list together.
*/
#ifndef ListH
#define ListH

// list_element is the type of data in a node of the linked list

typedef int list_element;

// DUMMY is a constant that is filled in for the dummy head and tail
// nodes

const list_element DUMMY=-9999;

// The node is the object that will be linked together in the list

struct node
{
  list_element data;
  node * next;
};

/* 
   The list is the class that builds the linked list out of nodes
   It provides 3 pointers, 
   - head which points to the dummy head node
   - tail which points to the dummy tail node
   - curr which is a temporary pointer that is moved up and down the
     list.  The curr pointer is moved directly by the first and next
     functions, and indirectly by the find and find_prior functions.
   NOTE the curr pointer should never be NULL, since we don't ever check
        for NULL.  Instead we always re-set curr with first();
 	(curr=head->next;) whenever we lose our current curr.
*/

class list
{
private:
  node * head, * tail, * curr;

  node * create_node(list_element e);	// creates a new node from an element
  void init_empty();			// build empty list
public:
  list() {init_empty();}
  list(list_element e) {init_empty(); add_front(e);}
  list(list &l) {init_empty(); copy(l);}
  ~list();

  void add_front(list_element x); // add to list 
  void add_sort(list_element x);
  void add_curr(list_element x);
  void add_tail(list_element x);

  void next(){curr=curr->next;}
  void first(){curr=head->next;}
  int find(list_element);	  // find matching element
  int find_prior(list_element);	  // find el. in front of matching el.
  void copy(list &l);		  // make self become a copy of l

  // returns the element at the curr location
  list_element get_data()
    {
      if (curr!=NULL)
        return curr->data;
      else
	return DUMMY;
    }

  int is_empty(){return (head->next == tail);}  // questions
  int is_last(){return (curr->next==tail);}
  int is_first(){return (curr==head->next);}

  void delete_curr();		// remove current element
  void clear();			// remove everything

  void print();			// print out whole list
};

#endif
