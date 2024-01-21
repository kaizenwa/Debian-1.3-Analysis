{
   GrVertex p1,p2,p3;
   struct vertex_buffer *VB=ctx->VB;
   GLint shift=ctx->ColorShift;

#if defined(DEBUG_FXMESA)
   printf("fxmesa: %s(...)\n",FUNCNAME);
#endif

#if defined(TRI_SMOOTH)
   VSETRGBA(p1,shift,v1);
   VSETRGBA(p2,shift,v2);
   VSETRGBA(p3,shift,v3);
#else
   grConstantColorValue(FXCOLOR(
	   VB->Color[pv][0]>>shift,
	   VB->Color[pv][1]>>shift,
	   VB->Color[pv][2]>>shift,
	   VB->Color[pv][3]>>shift));
#endif

   VSETXY(p1,v1);
   VSETXY(p2,v2);
   VSETXY(p3,v3);

#if defined(TRI_DEPTH) || defined(TRI_TEX)
   VSETZ(p1,v1);
   VSETZ(p2,v2);
   VSETZ(p3,v3);
#endif

#if defined(TRI_TEX)
   VSETST(p1,v1);
   VSETST(p2,v2);
   VSETST(p3,v3);
#endif

   FIXCOORD(p1);
   FIXCOORD(p2);
   FIXCOORD(p3);

   grDrawTriangle(&p1,&p2,&p3);
};
#undef TRI_SMOOTH
#undef TRI_DEPTH
#undef TRI_TEX
#undef FUNCNAME

