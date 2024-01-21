/*$Id: c_nodset.cc,v 11.38 96/03/24 17:59:20 al Exp $ -*- C++ -*-
 * nodeset and ic commands
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "l_lib.h"
#include "c_nodset.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void	CMD::ic(CS&);
//	void	CMD::nodeset(CS&);
//	void	NODESET::command(CS&,NODESET*);
//	void	NODESET::add_list(CS&);
//	void	NODESET::add1(int,double);
//	void	NODESET::rm_all()
//	void	NODESET::rm1(int);
//	void	NODESET::list_all()const
//	void	NODESET::list1()const
/*--------------------------------------------------------------------------*/
static NODESET iclist;
static NODESET nslist;
/*--------------------------------------------------------------------------*/
void CMD::ic(CS& cmd)
{
  iclist.command(cmd);
}
/*--------------------------------------------------------------------------*/
void CMD::nodeset(CS& cmd)
{
  nslist.command(cmd);
}
/*--------------------------------------------------------------------------*/
void NODESET::command(CS& cmd)
{
  if (cmd.end()){
    list_all();
  }else if (cmd.pmatch("CLEAR")){
    rm_all();
  }else{
    add_list(cmd);
  }
}
/*--------------------------------------------------------------------------*/
/* add_list: add a bunch of nodes
 */
void NODESET::add_list(CS& cmd)
{
  while (cmd.pmatch("V")){
    int paren = cmd.skiplparen();
    int node = cmd.ctoi();
    paren -= cmd.skiprparen();
    if (paren != 0){
      cmd.check(bWARNING);
    }
    cmd.skipequal();
    int mark = cmd.cursor();
    double voltage = cmd.ctof();
    if (mark == cmd.cursor()){		/* no voltage value = unset */
      rm1(node);
    }else{
      add1(node,voltage);
    }
  }
  cmd.check(bWARNING);
}
/*--------------------------------------------------------------------------*/
/* add1: add a new nodeset or ic
 */
void NODESET::add1(int node, double voltage)
{
  NODESET *ptr;
  
  for (ptr=this;  (ptr->Next) && (ptr->Next->Node < node);  ptr=ptr->Next)
    /* nothing */;
  
  if ((ptr->Next) && (ptr->Next->Node == node)){
    error(bWARNING, "replacing nodeset/ic at node %u\n", node);
    ptr = ptr->Next;
    ptr->Voltage = voltage;
  }else{
    NODESET *newnode;
    newnode = new NODESET(node, voltage, ptr->Next);
    ptr->Next = newnode;
  }
}  
/*--------------------------------------------------------------------------*/
/* rm_all: unset all nodes
 * but leave top allocated
 */
void NODESET::rm_all()
{
  while (Next){
    NODESET *link;
    link = Next->Next;
    delete Next;
    Next = link;
  }  
}
/*--------------------------------------------------------------------------*/
/* rm1:  (unset a node)
 * if the node to be removed isn't there, silently do nothing
 */
void NODESET::rm1(int node)
{
  NODESET *ptr;
  
  for (ptr=this;  (ptr->Next) && (ptr->Next->Node < node);  ptr=ptr->Next)
    /* nothing */;			/* scan for node match */
  
  if ((ptr->Next) && (ptr->Next->Node == node)){
    NODESET *link;
    link = ptr->Next->Next;
    delete ptr->Next;
    ptr->Next = link;
  }
}
/*--------------------------------------------------------------------------*/
/* list_all: list all nodes set
 */
void NODESET::list_all()const
{
  NODESET *ptr;
  for (ptr = this->Next;  ptr;  ptr = ptr->Next){
    ptr->list1();
  }  
  mprintf(IO::mstdout,"\n");	
}
/*--------------------------------------------------------------------------*/
/* list1: list 1 node set
 */
void NODESET::list1()const
{
  mprintf(IO::mstdout, " V(%d)=%s ", Node, ftos(Voltage,"",7,0));
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
