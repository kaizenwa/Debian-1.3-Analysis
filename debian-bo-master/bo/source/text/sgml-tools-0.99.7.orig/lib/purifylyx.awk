BEGIN {
	layout=0
	layout_line=""
}

($1 == "\\layout") {
	layout=1
	layout_line=$0
	next
}

($1 != "\\layout") {
	if (layout && NF) {
		print layout_line
		layout=0
	}
	print $0
}
