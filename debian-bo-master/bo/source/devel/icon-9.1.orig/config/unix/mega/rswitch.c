/*
** This is the co-expression context switch for the Convergent Technologies
** MegaFrame operating under CTIX (Unix V)
*/

/*
** coswitch
*/
coswitch( old_cs,new_cs,first )
int *old_cs, *new_cs ;
int first ;
{
   asm( "   mov.l  8(%fp),%a0" )  ;      /* a0 = old_cs */
   asm( "   mov.l  12(%fp),%a1" ) ;      /* a1 = new_cs */
   asm( "   mov.l  %sp,0(%a0)" )   ;
   asm( "   mov.l  %fp,4(%a0)" )   ;
   asm( "   mov.l  %a2,8(%a0)" )   ;
   asm( "   mov.l  %a3,12(%a0)" )  ;
   asm( "   mov.l  %a4,16(%a0)" )  ;
   asm( "   mov.l  %a5,20(%a0)" )  ;
   asm( "   mov.l  %d2,24(%a0)" )  ;
   asm( "   mov.l  %d3,28(%a0)" )  ;
   asm( "   mov.l  %d4,32(%a0)" )  ;
   asm( "   mov.l  %d5,36(%a0)" )  ;
   asm( "   mov.l  %d6,40(%a0)" )  ;
   asm( "   mov.l  %d7,44(%a0)" )  ;

   if (first == 0)
   {  /* this is the first activation */

      asm( "   mov.l  0(%a1),%sp" )  ;
      asm( "   mov.l  &0,%fp" )  ;
      new_context( 0,0 ) ;
      syserr( "new_context() returned in coswitch" ) ;
   }
   else
   {
      asm( "   mov.l  0(%a1),%sp" )   ;
      asm( "   mov.l  4(%a1),%fp" )   ;
      asm( "   mov.l  8(%a1),%a2" )   ;
      asm( "   mov.l  12(%a1),%a3" )  ;
      asm( "   mov.l  16(%a1),%a4" )  ;
      asm( "   mov.l  20(%a1),%a5" )  ;
      asm( "   mov.l  24(%a1),%d2" )  ;
      asm( "   mov.l  28(%a1),%d3" )  ;
      asm( "   mov.l  32(%a1),%d4" )  ;
      asm( "   mov.l  36(%a1),%d5" )  ;
      asm( "   mov.l  40(%a1),%d6" )  ;
      asm( "   mov.l  44(%a1),%d7" )  ;                    
   }
}                     
