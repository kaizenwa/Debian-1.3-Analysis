/*
   vllist.h
   
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

#ifndef _list_h_
#define _list_h_

struct VNode {

#ifdef DEBUG
  int magic1;
#endif
  
  void *obj;
  VNode *next;
  VNode *prev;

  VNode();
  ~VNode();
  
#ifdef DEBUG
  int magic2;
#endif  
};

/*
 * Doubly linked list of (void*)'s
 */
class VLList {

 public:
  VLList();
  virtual ~VLList();

  int add_head( void* );
  int add_tail( void* );

  void* pop_head();
  void* pop_tail();

  /*
   * These versions of pop can be used in the
   * middle of iteration:
   * 				  
   * 	pop_cur_n	Pops current item and advances iteration
   *                to `next' 
   * 	pop_cur_p	Pops current item and moves iteration
   *                to `prev' 
   */
  void* pop_cur_n();
  void* pop_cur_p();

  /* Returns `cur' */
  void *peek_cur();

  /* Slow, but sometimes convenient, to get item by index */
  void *at( int index );
  
  /* Get number of items */
  virtual int num_items();

  /* Iteration */
  void* go_head();	/* Returns head */
  void* go_tail();	/* Returns tail */

  void* go_next();	/* Returns next */
  void* go_prev();	/* Returns prev */

#ifdef DEBUG
  void verify_links( char *caller = "UNKNOWN" );
#endif
  
 private:
  VNode *head;
  VNode *tail;
  VNode *cur;	/* For iteration */
  int num;
};

#endif
  
  
