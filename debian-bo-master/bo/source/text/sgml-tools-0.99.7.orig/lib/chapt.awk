
# This awk script adds \documentstyle, \begin{document} and
# \end{document} TeX commands to the beginning and end of a file.

# possible improvement:  allow style options to be passed from the
# command line.

BEGIN {	print "\\documentstyle[qwertz,dina4,xlatin1]{report}"
        print "\\input{epsf.tex}"
	print "\\begin{document}" }
{ print }
END   { print "\\end{document}"   }


