# awk program to check newsgroup names for validity, rigorously
# echo groupname | awk -f namecheck.awk
# It may look like some things in per-component checking could be done
# more efficiently by moving them into whole-name checking.  The presence
# of encoded name components messes this up.
NR == 1 {
	# whole-name checks
	bad = 1
	if (NF == 0)
		print "empty name"
	else if (NF > 1)
		print "white space in name"
	else if ($1 ~ /^\./ || $1 ~ /\.$/ || $1 ~ /\.\./)
		print "bad dot(s) in name"
	else if ($1 !~ /^[a-zA-Z]/)	# A-Z is caught later
		print "name does not begin with a letter"
	else if ($1 ~ /^(junk|control)\./)
		print "name starts with control or junk"
	else
		bad = 0
	if (bad == 1)
		exit 1

	# per-component checks
	nc = split($1, cpt, ".")
	for (i = 1; i <= nc; i++) {
		bad = 1
		c = cpt[i]
		# some of these two-part tests may look like they could be
		# simplified to one-parters, but some broken awks then fail
		if (c ~ /^=/ && c !~ /^=\?[^=?]+\?[^=?]+\?[^?]+\?=$/)
			print "name component resembles encoded word but isn't one"
		else if (c ~ /^=/ && c !~ /^=[a-zA-Z0-9+_=?-]*=$/)
			print "bad character in encoded name component"
		else if (c ~ /^=/ && c !~ /^=\?[^?]+\?b\?/)
			print "encoded name component does not use b encoding"
		else if (c ~ /^=/)
			bad = 0		# looks like an okay encoded word
		else if (c ~ /^[0-9]*$/)
			print "all-numeric name component"
		else if (c !~ /^[a-zA-Z0-9]/)	# A-Z caught later
			print "name component starts with non-alphanumeric"
		else if (c !~ /[a-zA-Z]/)	# A-Z caught later
			print "name component does not contain letter"
		else if (c == "all" || c == "ctl")
			print "`all' or `ctl' used as name component"
		else if (length(c) > 14)
			print "name component longer than 14 characters"
		else if (c ~ /[A-Z]/)
			print "uppercase letter(s) in name"
		else if (c ~ /[^a-z0-9+_-]/)
			print "illegal character(s) in name"
		else if (c ~ /--|__|\+\+./)	# sigh, c++ etc must be allowed
			print "repeated punctuation in name"
		else if (c == cpt[i+1])
			print "repeated component(s) in name"
		else
			bad = 0

		if (bad == 1)
			exit 1
	}
}
NR >= 2 {
	print "newline(s) in name"
	exit 1
}
END {
	exit 0
}
