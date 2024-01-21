BEGIN	{
	  FS="\"";
	  print "/* ==> Do not modify this file!!  It is created automatically";
	  print "   by copying.awk.  Modify copying.awk instead.  <== */";
	  print ""
	  print "#include \"conq.h\""
	  print ""
	  print "void";
	  print "describe_copyright(arg, key, buf)";
	  print "int arg;";
	  print "char *key, *buf;";
	  print "{";
	}
NR == 1,/^[ 	]*NO WARRANTY[ 	]*$/	{
	  if ($0 ~ //)
	    {
	      printf "  tprintf(buf, \"\\n\");\n";
	    }
	  else if ($0 !~ /^[ 	]*NO WARRANTY[ 	]*$/) 
	    {
	      printf "  tprintf(buf, \"";
	      for (i = 1; i < NF; i++)
		printf "%s\\\"", $i;
	      printf "%s\\n\");\n", $NF;
	    }
	}
/^[	 ]*NO WARRANTY[ 	]*$/	{
	  print "}";
	  print "";
	  print "void";
	  print "describe_warranty(arg, key, buf)";
	  print "int arg;";
	  print "char *key, *buf;";
	  print "{";
	}
/^[ 	]*NO WARRANTY[ 	]*$/, /^[ 	]*END OF TERMS AND CONDITIONS[ 	]*$/{  
	  if (! ($0 ~ /^[ 	]*END OF TERMS AND CONDITIONS[ 	]*$/)) 
	    {
	      printf "  tprintf(buf, \"";
	      for (i = 1; i < NF; i++)
		printf "%s\\\"", $i;
	      printf "%s\\n\");\n", $NF;
	    }
	}
END	{
	  print "}";
	}
