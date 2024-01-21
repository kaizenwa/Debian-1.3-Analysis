
HTMLDOCS = \
    dtd2html.html \
    dtd_pl.html \
    dtddiff.html \
    dtdtree.html \
    perlSGML.html \
    sgml_pl.html \
    stripsgml.html

HTML2TXTCMD = lynx -dump -nolist -underscore

textdoc:
	chmod 644 $(HTMLDOCS)
	$(HTML2TXTCMD) dtd2html.html > dtd2html.txt
	$(HTML2TXTCMD) dtd_pl.html > dtd_pl.txt
	$(HTML2TXTCMD) dtddiff.html > dtddiff.txt
	$(HTML2TXTCMD) dtdtree.html > dtdtree.txt
	$(HTML2TXTCMD) perlSGML.html > perlSGML.txt
	$(HTML2TXTCMD) sgml_pl.html > sgml_pl.txt
	$(HTML2TXTCMD) stripsgml.html > stripsgml.txt
	chmod 644 *

