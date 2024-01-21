# WARNING
#
# This Makefile is NOT for installation purposes. Please read the file
# docs/Install.html for information about installing Tix.
#
#
#
#
#
#

# When a new manual page is created, do the following:
#   (1) Put it in this Makefile
#   (2) Put it in ../docs/ManPages.html

warning::
	@ head -10 Makefile
	@ exit 1

docs:: html $(HTML41)

# This will be overrided by some upper level Makefiles
# 
PATCH_LEVEL = 4.0
PSREF   = TixManPages$(PATCH_LEVEL).ps
PSTITLE = TixManTitle$(PATCH_LEVEL).ps

$(PSREF)::
	cp prolog/prolog.ps $(PSTITLE)
	groff -Tps -man *.1 *.n  > $(PSREF)

psdocs:: $(PSREF)

# This section is for makeing amd moving the Tix manual page into
# my html directory.
#

txt:


.SUFFIXES: .n .1 .3 .html

MAN_1 = tixwish.1
MAN_3 = 

MAN_N = Balloon.n BtnBox.n ComboBox.n Control.n DItem.n Destroy.n \
	DirList.n DirTree.n \
	EFileBox.n EFileDlg.n FileBox.n FileDlg.n FileEnt.n Form.n \
	GetBool.n GetInt.n HList.n InpOnly.n LabEntry.n LabFrame.n \
	ListNBK.n Mwm.n NBFrame.n\
	NoteBook.n OptMenu.n PanedWin.n PopMenu.n SHList.n SListBox.n \
	SText.n SWindow.n Select.n StdBBox.n \
	TixIntro.n Tree.n Utils.n Wm.n compound.n pixmap.n tix.n

MAN_N_41 = ChkList.n Meter.n Grid.n TList.n

man_1_html::    ${MAN_1:.1=.html}

man_3_html::    ${MAN_3:.3=.html}

man_n_html::    ${MAN_N:.n=.html}

man_n_html_41:: ${MAN_N_41:.n=.html}

html:: man_1_html man_3_html man_n_html

html41:: man_n_html_41

41doc:: html html41

.1.html:
	$(HOME)/bin/www/tk2html $< | more -s > $@

.3.html:
	$(HOME)/bin/www/tk2html $< | more -s > $@

.n.html:
	$(HOME)/bin/www/tk2html $< | more -s > $@

distclean:
	- rm -f *.a *.o core errs *~ \#* TAGS *.E a.out errors \
		*.ps

__copyright:
	for i in *.n *.1; \
	    do \
	    sed -e '/man\.macros/r copyright.txt' -e '/man\.macros/d' \
		    $$i > $$i.tmp; \
	    mv $$i.tmp $$i; \
	    done;

check:
	check.tcl

clean:: distclean
