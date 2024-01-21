files=sex.html sex-screenshot.gif sex-manual.html
htmldir=$(HOME)/public_html/SeX

sex-manual.html: sex-manual.html/index.html
sex-manual.html/index.html: sex-manual.sgml
	debiandoc2html sex-manual.sgml

sex-screenshot.gif: sex-screenshot.gif.uue
	uudecode sex-screenshot.gif.uue
	
install: $(files)
	cp -a $(files) $(htmldir)
	rm -f $(htmldir)/index.html
	mv $(htmldir)/sex.html $(htmldir)/index.html
	chmod -R go=u,go-w $(htmldir)/.

clean:
	rm -rf sex-manual.html sex-screenshot.gif
