/*
   vllist.cc

   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include "vllist.h"
#include "pool.h"

void do_fatal_error( char *file, int line, char *fmt, ... );
#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

#define MAX_NODES	1000

#define NODE_MAGIC1	0x8142d6ea
#define NODE_MAGIC2	0x17bd53ef

static Pool *node_pool = NULL;
static int num_insts = 0;		/* Number of VLLists */

#define ERROR_NOMEM() fatal( "Out of memory" )

static void *alloc1( void )
{
  VNode *node;

  node = new VNode;

  if ( !node )
	ERROR_NOMEM();
  
  return node;
}

static void free1( void *obj )
{
  delete (VNode*)(obj);
}

static int init_VLList_Class()
{
  node_pool = new Pool( "VNode", MAX_NODES, alloc1, free1 );

  if ( !node_pool )
	ERROR_NOMEM();
  
  num_insts = 0;
  return 0;
}

static int deinit_VLList_Class()
{
  delete node_pool;
  node_pool = NULL;
  return 0;
}

VNode::VNode()
{
#ifdef DEBUG  
  magic1 = NODE_MAGIC1;
  magic2 = NODE_MAGIC2;
#endif  
}

VNode::~VNode()
{
#ifdef DEBUG  
  if ( magic1 != NODE_MAGIC1 ||
	  magic2 != NODE_MAGIC2 )
	fatal( "VNode trashed" );

  magic1 = magic2 = 0;
#endif  
}

VLList::VLList()
{
  if ( node_pool == NULL )
	init_VLList_Class();
  
  head = tail = cur = NULL;
  num = 0;

  ++num_insts;
}

VLList::~VLList()
{
  VNode *v;

#ifdef DEBUG
  verify_links( "~VLList" );
#endif
  
  cur = head;

  while( cur )
	{
	  v = cur;
	  cur = cur->next;
	  node_pool->pfree( v );
	}

  if ( !--num_insts )
	deinit_VLList_Class();	 
}

int VLList::add_head( void *obj )
{
  VNode *node;

#ifdef DEBUG
  verify_links( "add_head" );
#endif
  
  cur = NULL;
  
  node = (VNode*)(node_pool->palloc());

  if ( !node )
	{
	  printf("VLList: can't get node\n");
	  return -1;
	}

  node->obj = obj;

  if ( !head )
	{
	  head = tail = node;
	  node->next = node->prev = NULL;
	  num = 1;
	}
  else
	{
	  head->prev = node;
	  node->next = head;
	  node->prev = NULL;
	  head = node;
	  ++num;
	}

  return 0;
}

int VLList::add_tail( void *obj )
{
  VNode *node;

#ifdef DEBUG
  verify_links( "add_tail" );
#endif
  
  cur = NULL;
  
  node = (VNode*)(node_pool->palloc());

  if ( !node )
	{
	  printf("VLList: can't get node\n");
	  return -1;
	}

  node->obj = obj;

  if ( !tail )
	{
	  head = tail = node;
	  node->next = node->prev = NULL;
	  num = 1;
	}
  else
	{
	  tail->next = node;
	  node->prev = tail;
	  node->next = NULL;
	  tail = node;
	  ++num;
	}

  return 0;
}

void *VLList::pop_head()
{
  VNode *node;
  void *obj;

#ifdef DEBUG
  verify_links( "pop_head" );
#endif
  
  cur = NULL;
  
  if ( !head )
	return NULL;

  node = head;

  if ( head == tail )
	{
	  head = tail = NULL;
	  num = 0;
	}
  else
	{
	  head = head->next;
	  head->prev = NULL;
	  --num;
	}

  obj = node->obj;
  node_pool->pfree( node );

  return obj;
}

void *VLList::pop_tail()
{
  VNode *node;
  void *obj;

#ifdef DEBUG
  verify_links( "pop_tail" );
#endif
  
  cur = NULL;
  
  if ( !tail )
	return NULL;

  node = tail;
  
  if ( head == tail )
	{
	  head = tail = NULL;
	  num = 0;
	}
  else
	{
	  tail = tail->prev;
	  tail->next = NULL;
	  --num;
	}

  obj = node->obj;
  node_pool->pfree( node );

  return obj;
}

int VLList::num_items()
{
#ifdef DEBUG
  verify_links( "num_items" );
#endif
  
  return num;
}

/* Pop (and return) current and advance current to next */
void *VLList::pop_cur_n()
{
  void *obj;
  VNode *node;

#ifdef DEBUG
  verify_links( "pop_cur_n" );
#endif
  
  if ( !cur )
	return NULL;

  if ( cur == head )
	{
	  obj = pop_head();
	  cur = head;
	}
  else if ( cur == tail )
	{
	  obj = pop_tail();
	  cur = NULL;
	}
  else	/* Node has both a `prev' and `next' */
	{
	  node = cur;
	  cur = cur->next;
	  node->prev->next = node->next;
	  node->next->prev = node->prev;
	  obj = node->obj;
	  node_pool->pfree( node );
	  --num;
	}

  return obj;
}

/* Pop (and return) current and advance current to prev */
void *VLList::pop_cur_p()
{
  void *obj;
  VNode *node;

#ifdef DEBUG
  verify_links( "pop_cur_n" );
#endif
  
  if ( !cur )
	return NULL;

  if ( cur == head )
	{
	  obj = pop_head();
	  cur = NULL;
	}
  else if ( cur == tail )
	{
	  obj = pop_tail();
	  cur = tail;
	}
  else	/* Node has both a `prev' and `next' */
	{
	  node = cur;
	  cur = cur->prev;
	  node->prev->next = node->next;
	  node->next->prev = node->prev;
	  obj = node->obj;
	  node_pool->pfree( node );
	  --num;
	}

  return obj;
}

void *VLList::peek_cur()
{
#ifdef DEBUG
  verify_links( "peek_cur" );
#endif
  
  return cur;
}

void *VLList::at( int index )
{
#ifdef DEBUG
  verify_links( "at" );
#endif
  
  if ( index < 0 || index >= num )
	fatal( "Index out of range.\n" );

  go_head();

  while( index-- )
	go_next();

  return cur->obj;
}

void *VLList::go_head()
{
#ifdef DEBUG
  verify_links( "go_head" );
#endif  
			  
  return (cur=head) ? head->obj : NULL;
}

void *VLList::go_tail()
{
#ifdef DEBUG
  verify_links( "go_tail" );
#endif
  
  return (cur=tail) ? cur->obj : NULL;
}

void *VLList::go_next()
{
#ifdef DEBUG
  verify_links( "go_next" );
#endif
  
  return (cur) ? ((cur=cur->next) ? cur->obj : NULL) : NULL;
}

void *VLList::go_prev()
{
#ifdef DEBUG
  verify_links( "go_prev" );
#endif
  
  return (cur) ? ((cur=cur->prev) ? cur->obj : NULL) : NULL;
}

#ifdef DEBUG
void VLList::verify_links( char *caller )
{
  int n;
  VNode *node;

  /* Go forwards */
  n = 0;
  node = head;
  while( node )
	{
	  if ( node->magic1 != NODE_MAGIC1 || node->magic2 != NODE_MAGIC2 )
		fatal( "VLList corrupted at node %d (forward)\nCaller `%s'",
			  n, caller );
	  ++n;
	  node = node->next;
	}

  if ( n != num )
	fatal( "`num' out of sync in VLList (expect %d - got %d) (forward)\nCaller `%s'", num, n, caller );

  /* Go backwards */
  n = 0;
  node = tail;
  while( node )
	{
	  if ( node->magic1 != NODE_MAGIC1 || node->magic2 != NODE_MAGIC2 )
		fatal( "VLList corrupted at node %d (backwards)\nCaller `%s'",
			 n, caller );
	  ++n;
	  node = node->prev;
	}

  if ( n != num )
	fatal( "`num' out of sync in VLList (expect %d - got %d) (backward)\nCaller `%s'",
		  num, n, caller );
}
#endif	/* DEBUG */	  


