# canonicalise the sys file:
# delete comments & leading whitespace, collapse continued lines
# rewritten to avoid assignment to $0, which is broken in older awks
# rewritten again (based on an idea from Charles Lindsey) to avoid
#  problems with awks that don't like very long print/printfs
BEGIN { midline = 0 }
/^#/ {				# comment
	if (midline)
		print "mid-line comment in sys" | "cat >&2"
	next
}
/^[\t ]*$/ {			# empty line
	if (midline)
		print "mid-line empty line in sys" | "cat >&2"
	next
}
{
	# strip leading white space
	for (n = 1; substr($0, n, 1) ~ /^[\t ]/; n++)
		{}
	thisln = substr($0, n)
}
/\\$/ {				# continued line
	printf "%s", substr(thisln, 1, length(thisln)-1)
	midline = 1
	next
}
{				# non-continued line
	print thisln
	midline = 0
	next
}
END {
	if (midline) {
		print "unterminated line in sys" | "cat >&2"
		printf "\n"
	}
}
