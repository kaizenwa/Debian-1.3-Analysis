$! OpenVMS Makefile.com - Written by Amit Margalit
$ cc ATTRIB.C
$ cc CRBUFFRI.C
$ cc CRBUFFRP.C
$ cc CRDATFRI.C
$ cc CRDATFRP.C
$ cc CREATE.C
$ cc CRIFRBUF.C
$ cc CRIFRDAT.C
$ cc CRIFRP.C
$ cc CRPFRBUF.C
$ cc CRPFRDAT.C
$ cc CRPFRI.C
$ cc DATA.C
$ cc HASHTAB.C
$ cc IMAGE.C
$ cc INFO.C
$ cc MISC.C
$ cc PARSE.C
$ cc RDFTOBUF.C
$ cc RDFTODAT.C
$ cc RDFTOI.C
$ cc RDFTOP.C
$ cc RGB.C
$ cc SCAN.C
$ cc SIMX.C
$ cc WRFFRBUF.C
$ cc WRFFRDAT.C
$ cc WRFFRI.C
$ cc WRFFRP.C
$! Done compiling
$! Now put in a library
$ library /create /object libxpm
$ library /object /insert libxpm ATTRIB.OBJ
$ library /object /insert libxpm CRBUFFRI.OBJ
$ library /object /insert libxpm CRBUFFRP.OBJ
$ library /object /insert libxpm CRDATFRI.OBJ
$ library /object /insert libxpm CRDATFRP.OBJ
$ library /object /insert libxpm CREATE.OBJ
$ library /object /insert libxpm CRIFRBUF.OBJ
$ library /object /insert libxpm CRIFRDAT.OBJ
$ library /object /insert libxpm CRIFRP.OBJ
$ library /object /insert libxpm CRPFRBUF.OBJ
$ library /object /insert libxpm CRPFRDAT.OBJ
$ library /object /insert libxpm CRPFRI.OBJ
$ library /object /insert libxpm DATA.OBJ
$ library /object /insert libxpm HASHTAB.OBJ
$ library /object /insert libxpm IMAGE.OBJ
$ library /object /insert libxpm INFO.OBJ
$ library /object /insert libxpm MISC.OBJ
$ library /object /insert libxpm PARSE.OBJ
$ library /object /insert libxpm RDFTOBUF.OBJ
$ library /object /insert libxpm RDFTODAT.OBJ
$ library /object /insert libxpm RDFTOI.OBJ
$ library /object /insert libxpm RDFTOP.OBJ
$ library /object /insert libxpm RGB.OBJ
$ library /object /insert libxpm SCAN.OBJ
$ library /object /insert libxpm SIMX.OBJ
$ library /object /insert libxpm WRFFRBUF.OBJ
$ library /object /insert libxpm WRFFRDAT.OBJ
$ library /object /insert libxpm WRFFRI.OBJ
$ library /object /insert libxpm WRFFRP.OBJ
