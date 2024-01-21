/*
 * File: oarith.r
 *  Contents: arithmetic operators + - * / % ^.  Auxiliary routines
 *   iipow, ripow.
 *
 * The arithmetic operators all follow a canonical conversion
 * protocol encapsulated in the macro ArithOp.
 */

int over_flow = 0;

#begdef ArithOp(icon_op, func_name, c_int_op, c_real_op)

   operator{1} icon_op func_name(x, y)
      declare {
#ifdef LargeInts
         tended struct descrip lx, ly;
#endif					/* LargeInts */
	 C_integer irslt;
         }
      arith_case (x, y) of {
         C_integer: {
            abstract {
               return integer
               }
            inline {
               extern int over_flow;
               c_int_op(x,y);
               }
            }
         integer: { /* large integers only */
            abstract {
               return integer
               }
            inline {
               big_ ## c_int_op(x,y);
               }
            }
         C_double: {
            abstract {
               return real
               }
            inline {
               c_real_op(x, y);
               }
            }
         }
end

#enddef

/*
 * x / y
 */

#begdef big_Divide(x,y)
{
  bigdiv(&x,&y,&result);
  return result;
}
#enddef
#define Divide(x,y) return C_integer (x / y);
#begdef RealDivide(x,y)
{
   double z;
#ifdef ZERODIVIDE
   if (y == 0.0)
      runerr(204);
#endif						/* ZERODIVIDE */
   z = x / y;
#ifdef SUN
   if (z >= HUGE || z <= -HUGE) {
      kill(getpid(), SIGFPE);
   }
#endif
   return C_double z;
}
#enddef


ArithOp( / , divide , Divide , RealDivide )

/*
 * x - y
 */

#begdef big_Sub(x,y)
{
   if (bigsub(&x,&y,&result) == Error) /* alcbignum failed */
      runerr(0);
   return result;
}
#enddef

#begdef Sub(x,y)
   irslt = sub(x,y);
   if (over_flow) {
#ifdef LargeInts
      MakeInt(x,&lx);
      MakeInt(y,&ly);
      if (bigsub(&lx,&ly,&result) == Error) /* alcbignum failed */
         runerr(0);
      return result;
#else					/* LargeInts */
      runerr(203);
#endif					/* LargeInts */
      }
   else return C_integer irslt;
#enddef

#define RealSub(x,y) return C_double (x - y);

ArithOp( - , minus , Sub , RealSub )


/*
 * x % y
 */

#define Abs(x) ((x) > 0 ? (x) : -(x))
/*
 * The sign of modulo's result must match that of x.
 */
#begdef MatchSignToX(x,y,theResult,zero)
   if (x < zero) {
      if (theResult > zero) {
         theResult -= Abs(y);
         }
      }
   else if (theResult < zero) {
      theResult += Abs(y);
      }
#enddef

#begdef big_IntMod(x,y)
{
   if (bigmod(&x,&y,&result) == Error)
      runerr(0);
   return result;
}
#enddef

#begdef IntMod(x,y)
{
   if (y == 0) {
      irunerr(202,y);
      errorfail;
      }
   irslt = x % y;
   MatchSignToX(x,y,irslt,0);
   return C_integer irslt;
}
#enddef

#begdef RealMod(x,y)
{
   double d;
   d = fmod(x, y);
   MatchSignToX(x,y,d,0.0);
   return C_double d;
}
#enddef

ArithOp( % , mod , IntMod , RealMod )

/*
 * x * y
 */

#begdef big_Mpy(x,y)
{
   if (bigmul(&x,&y,&result) == Error)
      runerr(0);
   return result;
}
#enddef

#begdef Mpy(x,y)
   irslt = mul(x,y);
   if (over_flow) {
#ifdef LargeInts
      MakeInt(x,&lx);
      MakeInt(y,&ly);
      if (bigmul(&lx,&ly,&result) == Error) /* alcbignum failed */
         runerr(0);
      return result;
#else					/* LargeInts */
      runerr(203);
#endif					/* LargeInts */
      }
   else return C_integer irslt;
#enddef


#define RealMpy(x,y) return C_double (x * y);

ArithOp( * , mult , Mpy , RealMpy )


"-x - negate x."

operator{1} - neg(x)
   if cnv:(exact)C_integer(x) then {
      abstract {
         return integer
         }
      inline {
	    C_integer i;
	    extern int over_flow;

	    i = neg(x);
	    if (over_flow) {
#ifdef LargeInts
	       struct descrip tmp;
	       MakeInt(x,&tmp);
	       if (bigneg(&tmp, &result) == Error)  /* alcbignum failed */
	          runerr(0);
               return result;
#else					/* LargeInts */
	       irunerr(203,x);
               errorfail;
#endif					/* LargeInts */
               }
         return C_integer i;
         }
      }
#ifdef LargeInts
   else if cnv:(exact) integer(x) then {
      abstract {
         return integer
         }
      inline {
	 if (cpbignum(&x, &result) == Error)  /* alcbignum failed */
	    runerr(0);
	 BlkLoc(result)->bignumblk.sign ^= 1;
	 return result;
         }
      }
#endif					/* LargeInts */
   else {
      if !cnv:C_double(x) then
         runerr(102, x)
      abstract {
         return real
         }
      inline {
         double drslt;
	 drslt = -x;
         return C_double drslt;
         }
      }
end


"+x - convert x to a number."
/*
 *  Operational definition: generate runerr if x is not numeric.
 */
operator{1} + number(x)
   if cnv:(exact)C_integer(x) then {
       abstract {
          return integer
          }
       inline {
          return C_integer x;
          }
      }
#ifdef LargeInts
   else if cnv:(exact) integer(x) then {
       abstract {
          return integer
          }
       inline {
          return x;
          }
      }
#endif					/* LargeInts */
   else if cnv:C_double(x) then {
       abstract {
          return real
          }
       inline {
          return C_double x;
          }
      }
   else
      runerr(102, x)
end

/*
 * x + y
 */

#begdef big_Add(x,y)
{
   if (bigadd(&x,&y,&result) == Error)
      runerr(0);
   return result;
}
#enddef

#begdef Add(x,y)
   irslt = add(x,y);
   if (over_flow) {
#ifdef LargeInts
      MakeInt(x,&lx);
      MakeInt(y,&ly);
      if (bigadd(&lx, &ly, &result) == Error)  /* alcbignum failed */
	 runerr(0);
      return result;
#else					/* LargeInts */
      runerr(203);
#endif					/* LargeInts */
      }
   else return C_integer irslt;
#enddef

#define RealAdd(x,y) return C_double (x + y);

ArithOp( + , plus , Add , RealAdd )


"x ^ y - raise x to the y power."

operator{1} ^ powr(x, y)
   if cnv:(exact)integer(y) then {
      if cnv:(exact)integer(x) then {
         abstract {
            return integer
            }
         inline {
	    extern int over_flow;
#ifdef LargeInts
            if (bigpow(&x, &y, &result) == Error)  /* alcbignum failed */
               runerr(0);
            return result;
#else
            C_integer r = iipow(IntVal(x), IntVal(y));
            if (over_flow)
               runerr(203);
            return C_integer r;
#endif
           }
         }
      else {
         if !cnv:C_double(x) then
            runerr(102, x)
         abstract {
            return real
            }
         inline {
            if (ripow(x,IntVal(y), &result) ==  Error)
                runerr(0);
            return result;
            }
         }
      }
   else {
      if !cnv:C_double(x) then
         runerr(102, x)
      if !cnv:C_double(y) then
         runerr(102, y)
      abstract {
         return real
         }
      inline {
         if (x == 0.0 && y < 0.0)
             runerr(204);
         if (x < 0.0)
            runerr(206);
         return C_double pow(x,y);
         }
      }
end

#if COMPILER || !(defined LargeInts)
/*
 * iipow - raise an integer to an integral power. 
 */
C_integer iipow(n1, n2)
C_integer n1, n2;
   {
   C_integer result;

   if (n1 == 0 && n2 <= 0) {
      over_flow = 1;
      return 0;
      }
   if (n2 < 0)
      return 0;
   result = 1L;
   while (n2 > 0) {
      if (n2 & 01L)
         result *= n1;
      n1 *= n1;
      n2 >>= 1;
      }
   over_flow = 0;
   return result;
   }
#endif					/* COMPILER || !(defined LargeInts) */


/*
 * ripow - raise a real number to an integral power.
 */
int ripow(r, n, drslt)
double r;
C_integer n;
dptr drslt;
   {
   double retval;

   if (r == 0.0 && n <= 0) 
      ReturnErrNum(204, Error);
   if (n < 0) {
      n = -n;
      r = 1.0 / r;
      }
   retval = 1.0;
   while (n > 0) {
      if (n & 01L)
         retval *= r;
      r *= r;
      n >>= 1;
      }
   Protect(BlkLoc(*drslt) = (union block *)alcreal(retval), return Error);
   drslt->dword = D_Real;
   return Succeeded;
   }
