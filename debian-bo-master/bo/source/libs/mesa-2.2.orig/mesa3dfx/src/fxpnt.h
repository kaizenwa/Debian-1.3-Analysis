{
   GLuint i;
   GrVertex p;
   GLint shift=ctx->ColorShift;
   struct vertex_buffer *VB=ctx->VB;

#if defined(DEBUG_FXMESA)
   printf("fxmesa: %s(...)\n",FUNCNAME);
#endif

#if !defined(PNT_SMOOTH)
   grConstantColorValue(fxMesa->color);
#endif

   for (i=first;i<=last;i++) {
      if (ctx->VB->Unclipped[i]) {
		  VSETXY(p,i);

#if defined(PNT_DEPTH) || defined(TRI_TEX)
		  VSETZ(p,i);
#endif

#if defined(PNT_TEX)
		  VSETST(p,i);
#endif

#if defined(PNT_SMOOTH)
		  VSETRGBA(p,shift,i);
#endif

		  grDrawPoint(&p);
      }
   }
}

#undef PNT_SMOOTH
#undef PNT_DEPTH
#undef PNT_TEX
#undef FUNCNAME
