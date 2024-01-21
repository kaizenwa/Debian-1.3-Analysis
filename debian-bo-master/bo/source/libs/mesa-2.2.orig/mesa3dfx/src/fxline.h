{
   GrVertex p1,p2;
   GLint shift=ctx->ColorShift;
   struct vertex_buffer *VB=ctx->VB;

#if defined(DEBUG_FXMESA)
   printf("fxmesa: %s(...)\n",FUNCNAME);
#endif

#if defined(LN_SMOOTH)
   VSETRGBA(p1,shift,v1);
   VSETRGBA(p2,shift,v2);
#else
   grConstantColorValue(FXCOLOR(
	   VB->Color[pv][0]>>shift,
	   VB->Color[pv][1]>>shift,
	   VB->Color[pv][2]>>shift,
	   VB->Color[pv][3]>>shift));
#endif

   VSETXY(p1,v1);
   VSETXY(p2,v2);

#if defined(LN_DEPTH) || defined(TRI_TEX)
   VSETZ(p1,v1);
   VSETZ(p2,v2);
#endif

#if defined(LN_TEX)
   VSETST(p1,v1);
   VSETST(p2,v2);
#endif

   FIXCOORD(p1);
   FIXCOORD(p2);

   grDrawLine(&p1,&p2);
};

#undef LN_SMOOTH
#undef LN_DEPTH
#undef LN_TEX
#undef FUNCNAME
