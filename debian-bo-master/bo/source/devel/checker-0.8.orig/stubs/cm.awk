$1=="/*" && $3=="*/" { GO=1;
   FILE=$2
   sub(/\.c/, "", FILE)
   RFILE=FILE
   sub(/stubs-/, "s-", RFILE)
}
/#if 0/ { GO-- }
/#endif/ { GO++ }
$1=="#define" { if (GO)
  {
    DEF=$2
    sub (/HAVE_/, "-", $2)
#    print FILE $2 ".c"
    print RFILE $2 ".o: $(srcdir)/stubs/" FILE ".c"
    print "\t$(CC) $(STUB_CFLAGS) -D" DEF " -o " RFILE $2 \
	  ".o -c $(srcdir)/stubs/" FILE ".c\n"
    STUBS_OBJS=STUBS_OBJS RFILE $2 ".o "
  }
}
END { print STUBS_OBJS }
