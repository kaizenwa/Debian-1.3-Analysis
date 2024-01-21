BEGIN {
	descrip=0
	descrip_line=""
}

($0 == "@descrip@") {
	if (descrip_line) {
		print descrip_line
		print ""
	}
	descrip_line=""
	descrip++
	next
}

($0 == "@/descrip@") {
	print descrip_line
	print ""
	descrip_line=""
	descrip--
	next
}

(descrip) {
	if ($1 != "\\layout") {
		descrip_line=descrip_line " " $0
		next
	} else {
		print descrip_line
		print ""
		descrip_line=""
	}
}

{ print }
