/***************************************************************
 *     $Id: McHandable.h,v 3.0 1997/02/04 17:48:56 bzfzoeck Exp $
 *
 *     $Log: McHandable.h,v $
 *     Revision 3.0  1997/02/04 17:48:56  bzfzoeck
 *     released Version 3.0
 *
 *     Revision 1.3  1996/12/16 10:44:49  bzfzoeck
 *     now really
 *
 * Revision 1.1  1996/06/25  08:53:32  bzfzoeck
 * .
 *
 * Revision 1.2  1996/06/07  15:05:43  bzfzoeck
 * Smart pointers are implemented.
 *
 * Revision 1.1.1.1  1996/06/05  15:33:18  bzfstall
 * McLib initial repository
 *
****************************************************************/
#ifndef MC_HANDABLE_H
#define MC_HANDABLE_H

#include <assert.h>
#include <stdio.h>


/// Base class for ref-counted objects.
class McHandable
{
  protected:

    /// Destructor is protected. Use unref() instead.
    virtual ~McHandable() { 
      assert(refcount==0);
    }

    /// Reference count.
    int refcount;

  public:
    /// Constructor.
    McHandable() : refcount(0) { }

    /// Adds a reference to an instance.
    void ref() {
	//printf("Ref\n");
	refcount++;
    }

    /*/ Removes a reference from an instance. Calls delete this, if
      this was the last ref.*/
    void unref() {
	//printf("unref\n");
	assert(refcount>0);
	if (--refcount==0)
	    delete this;
    }

    /// Removes a reference but doesn't destroy object.
    void unrefNoDelete() {
	assert(refcount>0);
	--refcount;
    }


};

#endif
