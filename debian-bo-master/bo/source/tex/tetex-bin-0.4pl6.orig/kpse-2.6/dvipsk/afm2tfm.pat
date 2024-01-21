*** ./ORIG/afm2tfm.c	Sun Sep 11 15:20:23 1994
--- ./afm2tfm.c	Fri Mar  3 16:01:00 1995
*************** char *staticligkern[] = {
*** 89,92 ****
--- 89,124 ----
     "% LIGKERN seven {} * ; * {} seven ; eight {} * ; * {} eight ;",
     "% LIGKERN nine {} * ; * {} nine ;",
+ 
+ /* Kern accented characters the same way as their base. */
+   "% LIGKERN Aacute <> A ; aacute <> a ;",
+   "% LIGKERN Acircumflex <> A ; acircumflex <> a ;",
+   "% LIGKERN Adieresis <> A ; adieresis <> a ;",
+   "% LIGKERN Agrave <> A ; agrave <> a ;",
+   "% LIGKERN Aring <> A ; aring <> a ;",
+   "% LIGKERN Atilde <> A ; atilde <> a ;",
+   "% LIGKERN Ccedilla <> C ; ccedilla <> c ;",
+   "% LIGKERN Eacute <> E ; eacute <> e ;",
+   "% LIGKERN Ecircumflex <> E ; ecircumflex <> e ;",
+   "% LIGKERN Edieresis <> E ; edieresis <> e ;",
+   "% LIGKERN Egrave <> E ; egrave <> e ;",
+   "% LIGKERN Iacute <> I ; iacute <> i ;",
+   "% LIGKERN Icircumflex <> I ; icircumflex <> i ;",
+   "% LIGKERN Idieresis <> I ; idieresis <> i ;",
+   "% LIGKERN Igrave <> I ; igrave <> i ;",
+   "% LIGKERN Ntilde <> N ; ntilde <> n ;",
+   "% LIGKERN Oacute <> O ; oacute <> o ;",
+   "% LIGKERN Ocircumflex <> O ; ocircumflex <> o ;",
+   "% LIGKERN Odieresis <> O ; odieresis <> o ;",
+   "% LIGKERN Ograve <> O ; ograve <> o ;",
+   "% LIGKERN Otilde <> O ; otilde <> o ;",
+   "% LIGKERN Scaron <> S ; scaron <> s ;",
+   "% LIGKERN Uacute <> U ; uacute <> u ;",
+   "% LIGKERN Ucircumflex <> U ; ucircumflex <> u ;",
+   "% LIGKERN Udieresis <> U ; udieresis <> u ;",
+   "% LIGKERN Ugrave <> U ; ugrave <> u ;",
+   "% LIGKERN Yacute <> Y ; yacute <> y ;",
+   "% LIGKERN Ydieresis <> Y ; ydieresis <> y ;",
+   "% LIGKERN Zcaron <> Z ; zcaron <> z ;",
+ 
  /*
   *   These next are only included for deficient afm files that
*************** struct adobeinfo {
*** 115,118 ****
--- 147,151 ----
     struct lig *ligs ;
     struct kern *kerns ;
+    struct adobeptr *kern_equivs ;
     struct pcc *pccs ;
     int wptr, hptr, dptr, iptr ;
*************** struct kern {
*** 139,142 ****
--- 172,179 ----
     int delta ;
  } ;
+ struct adobeptr {
+    struct adobeptr *next;
+    struct adobeinfo *ch;
+ };
  struct pcc {
     struct pcc *next ;
*************** newchar() {
*** 333,336 ****
--- 370,374 ----
     ai->ligs = NULL ;
     ai->kerns = NULL ;
+    ai->kern_equivs = NULL ;
     ai->pccs = NULL ;
     ai->next = adobechars ;
*************** int oldn, newn ;
*** 913,917 ****
  long checksum() {
     int i ;
!    long s1 = 0, s2 = 0 ;
     char *p ;
     struct adobeinfo *ai ;
--- 951,955 ----
  long checksum() {
     int i ;
!    unsigned long s1 = 0, s2 = 0 ;
     char *p ;
     struct adobeinfo *ai ;
*************** long checksum() {
*** 919,923 ****
     for (i=0; i<256; i++)
        if (0 != (ai=adobeptrs[i])) {
!          s1 = (s1 << 1) ^ ai->width ;
           for (p=ai->adobename; *p; p++)
  #ifndef VMCMS
--- 957,961 ----
     for (i=0; i<256; i++)
        if (0 != (ai=adobeptrs[i])) {
!          s1 = ((s1 << 1) ^ (s1>>31)) ^ ai->width ; /* cyclic left shift */
           for (p=ai->adobename; *p; p++)
  #ifndef VMCMS
*************** int c ;
*** 1181,1184 ****
--- 1219,1235 ----
  }
  
+ char vnamebuf[100];
+ char *
+ vname (c)
+     int c;
+ {
+   if (!forceoctal && ISALNUM (c)) {
+     vnamebuf[0] = 0;
+   } else {
+     sprintf (vnamebuf, " (comment %s)", texptrs[c]->adobename);
+   }
+   return vnamebuf;
+ }
+ 
  void
  writevpl()
*************** writevpl()
*** 1190,1193 ****
--- 1241,1245 ----
     register struct pcc *npcc ;
     struct adobeinfo *asucc, *asub, *api ;
+    struct adobeptr *kern_eq;
     int xoff, yoff, ht ;
     char unlabeled ;
*************** writevpl()
*** 1207,1217 ****
     {
        char tbuf[300] ;
! 
!       sprintf(tbuf, "%s + %s", outencoding->name,
  #ifndef VMCMS
!          codingscheme) ;
  #else
!          ebcodingscheme) ;
  #endif
        if (strlen(tbuf) > 39) {
           error("Coding scheme too long; shortening to 39 characters.") ;
--- 1259,1275 ----
     {
        char tbuf[300] ;
!       char *base_encoding = 
  #ifndef VMCMS
!          codingscheme ;
  #else
!          ebcodingscheme ;
  #endif
+       
+       if (strcmp (outencoding->name, base_encoding) == 0) {
+         sprintf(tbuf, "%s", outencoding->name);
+       } else {
+         sprintf(tbuf, "%s + %s", base_encoding, outencoding->name);
+       }
+       
        if (strlen(tbuf) > 39) {
           error("Coding scheme too long; shortening to 39 characters.") ;
*************** writevpl()
*** 1224,1228 ****
     voutln("(COMMENT DESIGNSIZE (1 em) IS IN POINTS)") ;
     voutln("(COMMENT OTHER DIMENSIONS ARE MULTIPLES OF DESIGNSIZE/1000)") ;
!    voutln2("(CHECKSUM O %lo)",cksum ^ 0xffffffff) ;
     if (boundarychar >= 0)
        voutln2("(BOUNDARYCHAR O %lo)", (unsigned long)boundarychar) ;
--- 1282,1287 ----
     voutln("(COMMENT DESIGNSIZE (1 em) IS IN POINTS)") ;
     voutln("(COMMENT OTHER DIMENSIONS ARE MULTIPLES OF DESIGNSIZE/1000)") ;
!    /* Let vptovf compute the checksum. */
!    /* voutln2("(CHECKSUM O %lo)",cksum ^ 0xffffffff) ; */
     if (boundarychar >= 0)
        voutln2("(BOUNDARYCHAR O %lo)", (unsigned long)boundarychar) ;
*************** writevpl()
*** 1241,1245 ****
     vleft() ; voutln("MAPFONT D 0");
     voutln2("(FONTNAME %s)", outname) ;
!    voutln2("(FONTCHECKSUM O %lo)", (unsigned long)cksum) ;
     vright() ;
     if (makevpl>1) {
--- 1300,1304 ----
     vleft() ; voutln("MAPFONT D 0");
     voutln2("(FONTNAME %s)", outname) ;
!    /* voutln2("(FONTCHECKSUM O %lo)", (unsigned long)cksum) ; */
     vright() ;
     if (makevpl>1) {
*************** writevpl()
*** 1247,1251 ****
        voutln2("(FONTNAME %s)", outname) ;
        voutln2("(FONTAT D %d)", (int)(1000.0*capheight+0.5)) ;
!       voutln2("(FONTCHECKSUM O %lo)", (unsigned long)cksum) ;
        vright() ;
     }
--- 1306,1310 ----
        voutln2("(FONTNAME %s)", outname) ;
        voutln2("(FONTAT D %d)", (int)(1000.0*capheight+0.5)) ;
!       /* voutln2("(FONTCHECKSUM O %lo)", (unsigned long)cksum) ; */
        vright() ;
     }
*************** writevpl()
*** 1286,1290 ****
                             if (unlabeled) {
                                for (j = ai->texnum; j >= 0; j = nexttex[j])
!                                  voutln2("(LABEL %s)", vchar(j)) ;
                                unlabeled = 0 ;
                             }
--- 1345,1349 ----
                             if (unlabeled) {
                                for (j = ai->texnum; j >= 0; j = nexttex[j])
!                                  voutln3("(LABEL %s)%s", vchar(j), vname(j)) ;
                                unlabeled = 0 ;
                             }
*************** writevpl()
*** 1303,1323 ****
                       if (unlabeled) {
                          for (k = ai->texnum; k >= 0; k = nexttex[k])
!                            voutln2("(LABEL %s)", vchar(k)) ;
                          unlabeled = 0 ;
                       }
                       if (uppercase[i]) {
                          if (lowercase[j]) {
                             for (k=lowercase[j]->texnum; k >= 0; k = nexttex[k])
!                               voutln3("(KRN %s R %.1f)", vchar(k),
!                                     capheight*nkern->delta) ;
!                         } else voutln3("(KRN %s R %.1f)",
!                                  vchar(j), capheight*nkern->delta) ;
                       } else {
!                         voutln3("(KRN %s R %d)", vchar(j),
!                                 nkern->delta) ;
                          if (lowercase[j])
                             for (k=lowercase[j]->texnum; k >= 0; k = nexttex[k])
!                               voutln3("(KRN %s R %.1f)", vchar(k),
!                                 capheight*nkern->delta) ;
                       }
                    }
--- 1362,1392 ----
                       if (unlabeled) {
                          for (k = ai->texnum; k >= 0; k = nexttex[k])
!                            voutln3("(LABEL %s)%s", vchar(k), vname(k)) ;
                          unlabeled = 0 ;
                       }
+                      /* If other characters have the same kerns as this
+                         one, output the label here.  This makes the TFM
+                         file much smaller than if we output all the
+                         kerns again under a different label.  */
+                      for (kern_eq = ai->kern_equivs; kern_eq;
+                           kern_eq = kern_eq->next) {
+                         k = kern_eq->ch->texnum;
+                         voutln3("(LABEL %s)%s", vchar(k), vname(k)) ;
+                      }
+                      ai->kern_equivs = 0; /* Only output those labels once. */
                       if (uppercase[i]) {
                          if (lowercase[j]) {
                             for (k=lowercase[j]->texnum; k >= 0; k = nexttex[k])
!                               voutln4("(KRN %s R %.1f)%s", vchar(k),
!                                     capheight*nkern->delta, vname(k)) ;
!                         } else voutln4("(KRN %s R %.1f)%s",
!                                  vchar(j), capheight*nkern->delta, vname(j)) ;
                       } else {
!                         voutln4("(KRN %s R %d)%s", vchar(j),
!                                 nkern->delta, vname(j)) ;
                          if (lowercase[j])
                             for (k=lowercase[j]->texnum; k >= 0; k = nexttex[k])
!                               voutln4("(KRN %s R %.1f)%s", vchar(k),
!                                 capheight*nkern->delta, vname(k)) ;
                       }
                    }
*************** writevpl()
*** 1329,1337 ****
     for (i=bc; i<=ec; i++)
        if (0 != (ai=texptrs[i])) {
!          vleft() ; fprintf(vplout, "CHARACTER %s", vchar(i)) ;
!          if (*vcharbuf=='C') {
!             voutln("") ;
!          } else
!             voutln2(" (comment %s)", ai->adobename) ;
           if (uppercase[i]) {
              ai=uppercase[i] ;
--- 1398,1402 ----
     for (i=bc; i<=ec; i++)
        if (0 != (ai=texptrs[i])) {
!          vleft() ; fprintf(vplout, "CHARACTER %s%s\n   ", vchar(i), vname(i)) ;
           if (uppercase[i]) {
              ai=uppercase[i] ;
*************** FILE *f ;
*** 1391,1395 ****
  {
     (void)fprintf(f,
!  "afm2tfm 7.8, Copyright 1990-93 by Radical Eye Software\n") ;
     (void)fprintf(f,
   "Usage: afm2tfm foo[.afm] [-O] [-u] [-v|-V bar[.vpl]]\n") ;
--- 1456,1460 ----
  {
     (void)fprintf(f,
!  "afm2tfm 7.9, Copyright 1990-93, 95 by Radical Eye Software\n") ;
     (void)fprintf(f,
   "Usage: afm2tfm foo[.afm] [-O] [-u] [-v|-V bar[.vpl]]\n") ;
*************** struct adobeinfo *ai ;
*** 1582,1585 ****
--- 1647,1669 ----
        ai->kerns = rmkernmatch(ai->kerns, s2) ;
  }
+ 
+ /* Make the kerning for character S1 equivalent to that for S2.
+    If either S1 or S2 do not exist, do nothing.
+    If S1 already has kerning, do nothing.  */
+ void
+ addkern (s1, s2)
+     char *s1, *s2;
+ {
+   struct adobeinfo *ai1 = findadobe (s1);
+   struct adobeinfo *ai2 = findadobe (s2);
+   if (ai1 && ai2 && !ai1->kerns) {
+     /* Put the new one at the head of the list, since order is immaterial.  */
+     struct adobeptr *ap 
+       = (struct adobeptr *) mymalloc((unsigned long)sizeof(struct adobeptr));
+     ap->next = ai2->kern_equivs;
+     ap->ch = ai1;
+     ai2->kern_equivs = ap;
+   }
+ }
  int sawligkern ;
  /*
*************** char *s ;
*** 1618,1621 ****
--- 1702,1707 ----
           if (n == 3 && strcmp(mlist[1], "{}") == 0) { /* rmkern command */
              rmkern(mlist[0], mlist[2], (struct adobeinfo *)0) ;
+          } else if (n == 3 && strcmp(mlist[1], "<>") == 0) { /* addkern */
+             addkern(mlist[0], mlist[2]) ;
           } else if (n == 3 && strcmp(mlist[0], "||") == 0 &&
                                strcmp(mlist[1], "=") == 0) { /* bc command */
