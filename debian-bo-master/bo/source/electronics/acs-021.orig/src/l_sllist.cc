/*
  Philip White, mod by Al Davis
  list.cpp
  class example
  This is the code for the list class.  The list class is a singularly
  linked list with a dummy head and tail node.  The nodes of this list
  are of type "node" which is a struct that contains an element of type
  "list_element" (which in this example is an integer) and a next
  pointer which is used to link the list together.
*/

#include <iostream.h>
#include "list.h"
/*------------------------------------------------------*/
// this creates a new node from an element (private)

node * list::create_node(list_element e)
{
  node * temp=new node;

  temp->data=e;
  temp->next=NULL;
  return temp;
}
/*------------------------------------------------------*/
// this builds the empty list with the head pointing to the tail
// and the tail pointing to NULL, and curr pointing to head->next;

void list::init_empty()
{
  head=create_node(DUMMY);
  tail=create_node(DUMMY);
  tail->next=NULL;
  head->next=tail;
  first();
}
/*------------------------------------------------------*/
// make myself a copy of the list l.  Note this deletes my list;

void list::copy(list &l)
{
  clear();

  curr=head;
  l.first();
  while(l.curr != l.tail)
    {
      add_curr(l.get_data());
      l.next();
      next();
    }
  first();
}
/*------------------------------------------------------*/
// adds the element after the head location

void list::add_front(list_element x)
{
  node * temp=create_node(x);

  temp->next=head->next;
  head->next=temp;
}
/*------------------------------------------------------*/
// adds the element after the curr location

void list::add_curr(list_element x)
{
  node * temp=create_node(x);

  temp->next=curr->next;
  curr->next=temp;
}
/*------------------------------------------------------*/
// adds the element at the correct sorted location ascending order

void list::add_sort(list_element x)
{
  find_prior(x);
  add_curr(x);
}
/*------------------------------------------------------*/
// adds the element prior the tail location

void list::add_tail(list_element x)
{
  node * temp=create_node(x);
  node * t=head;

  while (t->next!=tail)
    t=t->next;

  temp->next=t->next;
  t->next=temp;
}
/*------------------------------------------------------*/
// find the first node in the list which has data that matches x
// returns T/F if it could find the element or not

int list::find(list_element x)
{
  if (find_prior(x))
    {
      next();
      return 1;
    }
  else
    return 0;
}
/*------------------------------------------------------*/
// find the node in the list that is directly in front of the first 
// occurance of a node with 'x'
// returns T/F if it could find the element or not

int list::find_prior(list_element x)
{
  curr=head;
  while ((curr->next!=tail) && (curr->next->data < x))
    next();

  if (curr->next->data==x)
    return 1;
  else
    return 0;
}
/*------------------------------------------------------*/
// delete the node in the list at the curr location

void list::delete_curr()
{
  node * temp = curr;

  if (find_prior(temp->data))
    {
      curr->next=temp->next;
      delete temp;
    }
  first();
}
/*------------------------------------------------------*/
// clears out all the non-dummy elements in the list

void list::clear()
{
  node * temp;

  first();
  temp=curr;
  while(temp!=tail)
    {
      next();
      delete temp;
      temp=curr;
    }
  head->next=tail;
  first();
}
/*------------------------------------------------------*/
list::~list()
{
  clear();
  delete tail;
  delete head;
}
/*------------------------------------------------------*/
// prints out the list nicely

void list::print()
{
  node *temp=head->next;
  
  cout<<"(";
  if (temp!=tail)
    {
      cout<<temp->data;
      temp=temp->next;
    }

  while (temp!=tail)
    {
      cout<<", "<<temp->data;
      temp=temp->next;
    }
  cout<<")"<<endl;
}
/*------------------------------------------------------*/
/*------------------------------------------------------*/
