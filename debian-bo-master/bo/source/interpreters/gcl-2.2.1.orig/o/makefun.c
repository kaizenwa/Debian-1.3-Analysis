#include "include.h"
#include "funlink.h"

#define PADDR(i) ((char *)(sSPinit->s.s_dbind->fixa.fixa_self[Mfix(i)]))
/* eg:
MakeAfun(addr,F_ARGD(min,max,flags,ARGTYPES(a,b,c,d)),0);
MakeAfun(addr,F_ARGD(2,3,NONE,ARGTYPES(OO,OO,OO,OO)),0);
*/
object MakeAfun(addr,argd,data)
object data, (*addr) ();
unsigned int argd;
{int type = (F_ARG_FLAGS_P(argd,F_requires_fun_passed) ? t_closure : t_afun);
  object x = alloc_object(type);
  x->sfn.sfn_name = Cnil;
  x->sfn.sfn_self = addr;
  x->sfn.sfn_argd = argd;
  if (type == t_closure)
    { x->cl.cl_env = 0;
      x->cl.cl_envdim=0;}
  x->sfn.sfn_data = data;
  return x;
}
 

object
fSmakefun(sym,addr,argd)
object sym, (*addr) ();
unsigned int argd;
{object ans = MakeAfun(addr,argd,
		      (sSPmemory && sSPmemory->s.s_dbind &&
		       type_of(sSPmemory->s.s_dbind)==t_cfdata) ?
		      sSPmemory->s.s_dbind : 0);
 ans->sfn.sfn_name = sym;
 return ans;
}

object
ImakeClosure(addr,argd,n,va_alist)
     object (*addr)();
     int n,argd;
     va_dcl
{ object x = fSmakefun(Cnil,addr,argd);
  va_list ap;
  va_start(ap);
  IsetClosure(x,n,ap);
  va_end(ap);
  return x;
}
     
IsetClosure(x,n,ap)
object x;
int n; va_list ap;
{  /* this will change so that we can allocate 'hunks' which will be little
   blocks the size of an array header say with only one header word.   This
   will be more economical. Because of gc, we can't allocate relblock, it
   might move while in the closure.  */
  object *p;
  if (type_of(x) != t_closure)
    { FEerror("Not a closure",0);}
  if (x->cl.cl_envdim < n)
    {BEGIN_NO_INTERRUPT; x->cl.cl_env = (object *)alloc_contblock(n);
     x->cl.cl_envdim = n;
     END_NO_INTERRUPT;
   }
  p = x->cl.cl_env;
  while (--n >= 0)
    { *p++ = va_arg(ap,object);
    }
}

DEFUN("INITFUN",object,fSinitfun,SI,3,ARG_LIMIT,NONE,OO,OO,OO,OO,
      "Store a compiled function on SYMBOL whose body is in the VV array at \
INDEX, and whose argd descriptor is ARGD.  If more arguments IND1, IND2,.. \
are supplied these are indices in the VV array for the environment of this \
closure.")
     (sym,addr_ind,argd,va_alist)
object sym,addr_ind,argd; va_dcl
{ int nargs = F_NARGS(VFUN_NARGS) -3;
  va_list ap;
  object fun = fSmakefun(IisSymbol(sym),PADDR(addr_ind),Mfix(argd));
  if (nargs > 0)
    { va_start(ap);
      IsetClosure(fun,nargs,ap);
      while (--nargs >= 0)
	/* the things put in by IsetClosure were only the indices
	   of the closure variables not the actual variables */
	{ fun->cl.cl_env[nargs]= (object) PADDR(fun->cl.cl_env[nargs]);}
      va_end(ap);
    }
  fSfset(sym,fun);
  return sym;
}

DEFUN("INITMACRO",object,fSinitmacro,SI,3,ARG_LIMIT,NONE,OO,OO,OO,OO,
      "Like INITFUN, but makes then sets the 'macro' flag on this symbol")
 (va_alist)
va_dcl     
{va_list ap;
 object res,name;
 va_start(ap);
 res = Iapply_ap(fSinitfun,ap);
 va_end(ap);
 res->s.s_mflag = 1;
 return res;
}

DEFUN("SET-KEY-STRUCT",object,fSset_key_struct,SI,1,1,NONE,OO,OO,OO,OO,
      "Called inside the loader.  The keystruct is set up in the file with \
   indexes rather than the actual entries.  We change these indices to \
   the objects")
     (key_struct_ind)
object key_struct_ind;

{ set_key_struct(PADDR(key_struct_ind),sSPmemory->s.s_dbind);
  return Cnil;
}
     

void
SI_makefun(strg,fn,argd)
     char *strg;
     unsigned int argd;
     object (*fn)();
{ object sym = make_si_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
}

void
LISP_makefun(strg,fn,argd)
     char *strg;
     unsigned int argd;
     object (*fn)();
{ object sym = make_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
}


object 
MakeClosure(n,argd,data,fn,va_alist)
int n;
object (*fn)();
object data;
va_dcl
{ object x;
  va_list ap;
  x = alloc_object(t_closure);
  x->cl.cl_name = Cnil;
  x->cl.cl_self = fn;
  x->cl.cl_data = data;
  x->cl.cl_argd = argd;
  x->cl.cl_env = 0;
  x->cl.cl_env = (object *)alloc_contblock(n*sizeof(object));
  x->cl.cl_envdim=n;
  va_start(ap);
  { object *p = x->cl.cl_env;
  while (--n>= 0)
    { *p++ = va_arg(ap,object);}
  va_end(ap);
  }
  return x;
}
      
DEFUN("INVOKE",object,fSinvoke,SI,1,ARG_LIMIT,NONE,OO,OO,OO,OO,
      "Invoke a C function whose body is at INDEX in the VV array")
 (x)
object x;
{ int (*fn)();
  fn = (void *) PADDR(x);
  (*fn)();
  return Cnil;
}
  
