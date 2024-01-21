# @(#) mkdriv.awk,v 1.5 1996/02/16 15:00:59 woods Exp

#    Copyright (C) 1987, 1988 by Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# awk program used from the mkdrivtab.sh shell script in building the
# drivertab.c file and makefiles for the directors/, routers/,
# transports/, lib/ and lookup/ directories.
#
# this awk file takes care of parsing the file and producing on the
# stdout directives and text which the shell can interpret to build the
# correct files.  The directives are always of the form '%string' and
# are used to indicate when the shell script should change state.
#
# See mkdrivtab.sh for details on the input format and on how this awk
# script is used.

BEGIN {
	n_director = 0
	n_router = 0
	n_transport = 0
	n_lookup = 0
	n_dlib = 0
	n_rlib = 0
	n_tlib = 0
	n_llib = 0
	lheader[" "] = ""
}

$2 == "library" {
	type = $1; d = 0; r = 0; t = 0; l = 0

	if (type == "director") d = 1
	else if (type == "router") r = 1
	else if (type == "transport") t = 1
	else if (type == "lookup") l = 1
	for (k = 3; k <= NF; k++) {
		if ($k ~ /^.*\.c$/) {
			if      (d) dlib[++n_dlib] = $k
			else if (r) rlib[++n_rlib] = $k
			else if (t) tlib[++n_tlib] = $k
			else if (l) llib[++n_llib] = $k
		} else if ($k ~ /^.*\.h$/) {
			if	(d) dheader[$k] = $k
			else if (r) rheader[$k] = $k
			else if (t) theader[$k] = $k
			else if (l) lheader[$k] = $k
		}
	}
	next
}

$1 == "director" || $1 == "router" || $1 == "transport" {
	type = $1; d = 0; r = 0; t = 0

	if (type == "director") {
		i = ++n_director
		d = 1
		prefix = "dt"
	} else if (type == "router") {
		i = ++n_router
		r = 1
		prefix = "rt"
	} else if (type == "transport") {
		i = ++n_transport
		t = 1
		prefix = "tp"
	}
	source = ""
	header = ""
	name = $2
	cache = prefix "c_" name
	driver = prefix "d_" name
	verify = prefix "v_" name
	finish = prefix "f_" name
	builder = prefix "b_" name
	dumper = prefix "p_" name
	for (k = 3; k <= NF; k++) {
		if      ($k == "nofinish")  finish = "NULL"
		else if ($k == "nocache")   cache = "NULL"
		else if ($k == "nobuilder") builder = "NULL"
		else if ($k == "nodumper")  dumper = "NULL"
		else if ($k ~ /^source=..*\.c$/) source = substr($k, 8)
		else if ($k ~ /^header=./)  header = substr($k, 8)
	}
	if (! source) source = name ".c"
	if (! header) header = substr(source, 1, length(source)-2) ".h"
	if (d) {
		dname[i] = name
		dcache[i] = cache
		ddriver[i] = driver
		dverify[i] = verify
		dfinish[i] = finish
		dbuilder[i] = builder
		ddumper[i] = dumper
		dsource[source] = source
		dheader[header] = header
	} else if (r) {
		rname[i] = name
		rcache[i] = cache
		rdriver[i] = driver
		rverify[i] = verify
		rfinish[i] = finish
		rbuilder[i] = builder
		rdumper[i] = dumper
		rsource[source] = source
		rheader[header] = header
	} else if (t) {
		tname[i] = name
		tcache[i] = cache
		tdriver[i] = driver
		tverify[i] = verify
		tfinish[i] = finish
		tbuilder[i] = builder
		tdumper[i] = dumper
		tsource[source] = source
		theader[header] = header
	}
}

$1 == "lookup" {
	name = $2
	lname[++n_lookup] = name
	source = ""
	header = ""
	for (k = 3; k <= NF; k++) {
		if ($k ~ /^source=..*\.c$/) source = substr($k, 8)
		else if ($k ~ /^header=./)  header = substr($k, 8)
	}
	if (! source) source = name ".c"
	lsource[source] = source
	if (header) lheader[header] = header
}

END {
	for (i = 1; i <= n_director; i++) {
		print "%CC%" dcache[i] "();"
		print "%DD%" ddriver[i] "();"
		print "%VV%" dverify[i] "();"
		print "%FF%" dfinish[i] "();"
		print "%BB%" dbuilder[i] "();"
		print "%PP%" ddumper[i] "();"
	}
	for (i = 1; i <= n_router; i++) {
		print "%CC%" rcache[i] "();"
		print "%RD%" rdriver[i] "();"
		print "%VV%" rverify[i] "();"
		print "%FF%" rfinish[i] "();"
		print "%BB%" rbuilder[i] "();"
		print "%PP%" rdumper[i] "();"
	}
	for (i = 1; i <= n_transport; i++) {
		print "%CC%" tcache[i] "();"
		print "%TD%" tdriver[i] "();"
		print "%FF%" tfinish[i] "();"
		print "%BB%" tbuilder[i] "();"
		print "%PP%" tdumper[i] "();"
	}
	for (i = 1; i <= n_lookup; i++) {
		print "%LO%" lname[i] "_open" "();"
		print "%LC%" lname[i] "_close" "();"
		print "%LL%" lname[i] "_lookup" "();"
	}
	print ""
	if (n_director) {
		print "struct direct_driver direct_drivers[] = {"
		for (i = 1; i <= n_director; i++) {
			printf("%%drop { \"%s\",%s,%s,%s,%s,%s,%s },\n",	\
				dname[i], dcache[i], ddriver[i],\
				dverify[i], dfinish[i], dbuilder[i], ddumper[i])
		}
		printf "%%drop { NULL },\n};\n\n"
	}
	if (n_router) {
		print "struct route_driver route_drivers[] = {"
		for (i = 1; i <= n_router; i++) {
			printf("%%drop { \"%s\",%s,%s,%s,%s,%s,%s },\n",	\
				rname[i], rcache[i], rdriver[i],\
				rverify[i], rfinish[i], rbuilder[i], rdumper[i])
		}
		printf "%%drop { NULL },\n};\n\n"
	}
	if (n_transport) {
		print "struct transport_driver transport_drivers[] = {"
		for (i = 1; i <= n_transport; i++) {
			printf("%%drop { \"%s\",%s,%s,%s,%s,%s },\n",	\
				tname[i], tcache[i], tdriver[i],\
				tfinish[i], tbuilder[i], tdumper[i])
		}
		printf "%%drop { NULL },\n};\n\n"
	}
	if (n_lookup) {
		print "struct lookup_proto lookup_protos[] = {"
		for (i = 1; i <= n_lookup; i++) {
			printf("%%drop ");
			printf("{ \"%s\", %s_open, %s_close, %s_lookup },\n", \
				lname[i], lname[i], lname[i], lname[i])
		}
		printf "%%drop { NULL },\n};\n"
	}

	for (step = 0; step < 4; step++) {
		d = 0; r = 0; t = 0; l = 0
		if (step == 0) {
			if (n_director == 0) continue;
			d = 1
			type = "directors"
			target = "ddrivlib"
		} else if (step == 1) {
			if (n_router == 0) continue;
			r = 1
			type = "routers"
			target = "rdrivlib"
		} else if (step == 2) {
			if (n_transport == 0) continue;
			t = 1
			type = "transports"
			target = "tdrivlib"
		} else {
			if (n_lookup == 0) continue;
			l = 1
			type = "lookup"
			target = "ldrivlib"
		}
		print "%makefile_start " type
		print "TARGET=" target ".a"
		printf("OBJ=")
		if (d) {
			for (s in dsource) {
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
			for (i = 1; i <= n_dlib; i++) {
				s = dlib[i]
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
		}
		if (r) {
			for (s in rsource) {
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
			for (i = 1; i <= n_rlib; i++) {
				s = rlib[i]
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
		}
		if (t) {
			for (s in tsource) {
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
			for (i = 1; i <= n_tlib; i++) {
				s = tlib[i]
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
		}
		if (l) {
			for (s in lsource) {
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
			for (i = 1; i <= n_llib; i++) {
				s = llib[i]
				printf(" %s.o", substr(s, 1, length(s)-2))
			}
		}
		printf("\nCSRC=")
		if (d) {
			for (s in dsource) {
				printf(" %s", s)
			}
			for (i = 1; i <= n_dlib; i++) {
				printf(" %s", dlib[i])
			}
		}
		if (r) {
			for (s in rsource) {
				printf(" %s", s)
			}
			for (i = 1; i <= n_rlib; i++) {
				printf(" %s", rlib[i])
			}
		}
		if (t) {
			for (s in tsource) {
				printf(" %s", s)
			}
			for (i = 1; i <= n_tlib; i++) {
				printf(" %s", tlib[i])
			}
		}
		if (l) {
			for (s in lsource) {
				printf(" %s", s)
			}
			for (i = 1; i <= n_llib; i++) {
				printf(" %s", llib[i])
			}
		}
		printf("\nHSRC=")
		if (d) for (s in dheader) {
			printf(" %s", s)
		}
		if (r) for (s in rheader) {
			printf(" %s", s)
		}
		if (t) for (s in theader) {
			printf(" %s", s)
		}
		if (l) for (s in lheader) {
			if (s != " ") {
				printf(" %s", s)
			}
		}
		printf("\n%%makefile_end\n")
	}
}
