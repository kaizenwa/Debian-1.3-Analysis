#!/bin/sh

CTAN=ftp://ftp.dante.de/tex-archive/

echo '<TITLE>The TeX System</TITLE>'
echo '<H1>The TeX System</H1>'

for i in *; do
	if [ $i = help ]; then
		echo "<h2>$i</h2><P>"
		echo '<ul>'
		for j in help/*; do
			if [ -f $j/index.html ]; then
				echo "<li><a href=\"$j/index.html\">`basename $j`</a>"
			else
				echo "<li><a href=\"$j\">`basename $j`</a>"
			fi
		done
		echo '</ul>'
		continue
	fi
	if [ -d $i ]; then
		echo "<h2>$i</h2><P>"
		echo '<ul>'
		for j in $i/*.html $i/*.html.gz $i/*.dvi $i/*.ps $i/*.tex $i/*.dvi.gz $i/*.ps.gz $i/*.tex.gz; do
			test -f $j && echo "<li><a href=\"$j\">`basename $j`</a>"
		done
		for j in $i/*; do
			test -d $j || continue
			test -f `echo $j/*.html | sed 's/ .*//'` ||
			test -f `echo $j/*.html.gz | sed 's/ .*//'` ||
			test -f `echo $j/*.ps.gz | sed 's/ .*//'` ||
			test -f `echo $j/*.dvi.gz | sed 's/ .*//'` ||
			test -f `echo $j/*.dvi | sed 's/ .*//'` ||
			test -f `echo $j/*.ps | sed 's/ .*//'` || continue
			echo '<li>'`basename $j`
			echo '<ul>'
			for k in $i/*.html $i/*.html.gz $j/*.dvi $j/*.ps $j/*.tex $j/*.dvi.gz $j/*.ps.gz $j/*.tex.gz; do
				test -f $k || continue
				echo "<li><a href=\"$k\">`basename $k`</a>"
			done
			echo '</ul>'
		done
		echo '</ul>'
	fi
done | awk '
initializing { nf = split($1,nm," ");
        $1 = "";
        nfa[nm[1]] = nf;
        if (nf) {
          helps[nm[1]] = $0;
          all[nm[1],1] = nm[1];
          for (i=2; i<=nf; ++i) {
            if (!(nm[i] in helps))
              helps[nm[i]] = "";
            all[nm[1],i] = nm[i];
          }
        }
        next
}

/^<li><a href=[^>]*>[^<>]*<\/a>$/ {
        str = $0;
        sub(/^<li><a href=[^>]*>/,"",str);
        sub(/<.*/,"",str);
        if (str in helps) {
          hstr = helps[str];
          sub (/^<li>/,"", $0);
          nf = nfa[str];
          for (i=1; i<=nf; ++i) {
            arg = $0;
            gsub(str, all[str,i], arg);
            gsub(all[str,i], arg, hstr);
          }
          print hstr;
        } else print;
        next;
}
{ print $0 }
' initializing=1 RS="" FS="\n" OFS="\n" helpfile initializing=0 RS="\n" FS=" " OFS=" " -

if test -w help/Catalogue/catalogue.html && grep '\.\./\.\./' help/Catalogue/catalogue.html >/dev/null 2>&1; then
  echo "Changing CTAN location from local to $CTAN in catalogue.html..." >&2
  sed 's@\.\./\.\./@'"$CTAN"@g help/Catalogue/catalogue.html > .tmp$$ || rm -f .tmp$$
  test -f .tmp$$ && cat .tmp$$ > help/Catalogue/catalogue.html
  rm -f .tmp$$
fi

if test -w help/Catalogue/catalogue.html && grep 'http://www.ausflag.com.au/ausflag/images/' help/Catalogue/catalogue.html >/dev/null 2>&1; then
  echo "Changing location for 93-2.gif and 95-3.gif from www.ausflag.com.au to local in catalogue.html..." >&2
  sed 's@http://www.ausflag.com.au/ausflag/images/@@g' help/Catalogue/catalogue.html > .tmp$$ || rm -f .tmp$$
  test -f .tmp$$ && cat .tmp$$ > help/Catalogue/catalogue.html
  rm -f .tmp$$
fi

exit 0
