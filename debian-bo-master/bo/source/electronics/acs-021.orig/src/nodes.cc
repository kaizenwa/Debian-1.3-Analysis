/*$Id: nodes.cc,v 11.22 96/02/18 11:44:23 al Exp $ -*- C++ -*-
 * functions to handle node mapping
 */
#include "io.h"
#include "u_status.h"
/*--------------------------------------------------------------------------*/
	int	newnode_subckt(void);
	int	newnode_model(void);
/*--------------------------------------------------------------------------*/
/* newnode_subckt: get a new node number, unique internal nodes in subckts
 */
int newnode_subckt(void)
{
  ++STATUS::subckt_nodes;
  return ++STATUS::total_nodes;
}
/*--------------------------------------------------------------------------*/
/* newnode_model: get a new node number, unique internal nodes in models
 */
int newnode_model(void)
{
  ++STATUS::model_nodes;
  return ++STATUS::total_nodes;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
