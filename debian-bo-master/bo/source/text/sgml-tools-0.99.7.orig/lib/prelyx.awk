BEGIN {
	ident_level=0
	verbatim=0
}

($0 == "@enumerate@") {
	identlevel++
	if (ident_level > 1) {
		print ""
		print "\\begin_deeper"
		print "" }
	list_array[ident_level]="Enumerate"
	next
}

($0 == "@/enumerate@") {
	if (ident_level > 1) {
		print ""
		print "\\end_deeper"
		print "" }
	identlevel--
	next
}

($0 == "@itemize@") {
	identlevel++
	if (ident_level > 1) {
		print ""
		print "\\begin_deeper"
		print "" }
	list_array[ident_level]="Itemize"
	next
}

($0 == "@/itemize@") {
	if (ident_level > 1) {
		print ""
		print "\\end_deeper"
		print "" }
	identlevel--
	next
}

($0 == "@item@") {
	print "\\layout " list_array[ident_level]
	next
}

($0 == "@verb@") {
	verbatim=1
	print "\\layout Verbatim"
	print ""
	next
}

($0 == "@/verb@") {
	verbatim=0
	print "\\layout Standard"
	print ""
	next
}

{ if (verbatim) { print $0 "\\newline" }
	else { print }
}
