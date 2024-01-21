
#include "Expression.h"
#include "ExpressionList.h"

void ExpressionTypeEmit(ExpressionType *this)
{
  this->Emit(this);
}


/* 
 *  Swap if needed to make the type of 'this' "hold" more than the type of 
 *  'that'.  i.e.  if 'this' is integer and 'that' is float  -- then swap
 *  This is similar to the concept of automatic casting in C expressions 
 *  such as (int) + (float) will evaluate to (float).
*/ 
void ExpressionHierarchy(ExpressionType **this, ExpressionType **that)
{
  int Order[] = { 0,  11, 12, 0, 0, 0, 0, 10, 0, 0, 0,
		       0,  0, 0, 0, 0, 0,  0, 0, 0, 0,
		       0,  0, 0, 0, 0, 0,  0, 0, 0, 0, };

  if (((*this)->type >= MrmRtypeMax) || ((*that)->type >= MrmRtypeMax) ||
    ((*this)->type <= 0) || ((*that)->type <= 0)) { 
      Exit(LOC, "Can't Evaluate\n");
    }
  if (Order[(*this)->type] > Order[(*that)->type]) {
    ExpressionType *temp;
    
    temp = *this;
    *this = *that;
    *that = temp;
    return;
  }
}
    
void ExpressionTypeAdd(ExpressionType *this, ExpressionType *that)
{
  if (MrmRtypeAddrName == this->type)
    ExpressionListLookup(&this);
  if (MrmRtypeAddrName == that->type)
    ExpressionListLookup(&that);
  ExpressionHierarchy(&this, &that);
  this->value += that->value;
}

void ExpressionTypeSubtract(ExpressionType *this, ExpressionType *that)
{
  if (MrmRtypeAddrName == this->type)
    ExpressionListLookup(&this);
  if (MrmRtypeAddrName == that->type)
    ExpressionListLookup(&that);
  ExpressionHierarchy(&this, &that);
  this->value -= that->value;
}

void ExpressionTypeMultiply(ExpressionType *this, ExpressionType *that)
{
  if (MrmRtypeAddrName == this->type)
    ExpressionListLookup(&this);
  if (MrmRtypeAddrName == that->type)
    ExpressionListLookup(&that);
  ExpressionHierarchy(&this, &that); 
  this->value *= that->value;
}

void ExpressionTypeDivide(ExpressionType *this, ExpressionType *that)
{
  if (MrmRtypeAddrName == this->type)
    ExpressionListLookup(&this);
  if (MrmRtypeAddrName == that->type)
    ExpressionListLookup(&that);
  ExpressionHierarchy(&this, &that);
  this->value /= that->value;
}

void ExpressionTypeDelete(ExpressionType *this)
{
  /* free(this);  */
}


