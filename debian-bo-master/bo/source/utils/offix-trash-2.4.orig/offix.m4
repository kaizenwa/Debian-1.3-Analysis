# serial 1
offixdir='$(datadir)/OffiX'
AC_SUBST(offixdir)dnl
dnl ---- OFFIX_CHECK_DND 
dnl ----- checks for OffiX/DragAndDrop.h, libDnd.a andlibDnd++.a
dnl ----- in /usr/local/{include,lib} and ../Dnd
dnl ----- sets offix_dnd_includes, offix_dnd_libraries, CPPFLAGS and LDFLAGS
dnl ----- it is also possible to use --with constructs
AC_DEFUN(OFFIX_CHECK_DND,
[
offix_dnd_includes=""
offix_dnd_libraries=""
AC_ARG_WITH(dnd-inc,
    [  ]--with-dnd-inc=value    where to find Dnd includes,
    [CPPFLAGS="$CPPFLAGS -I$withval" offix_dnd_includes=$withval],)
AC_ARG_WITH(dnd-lib,
    [  ]--with-dnd-lib=value    where to find libDnd and libDnd++,
    [LDFLAGS="$LDFLAGS -L$withval" offix_dnd_libraries=$withval],)
offix_this_dir=`pwd`
#find the includes
if test "$offix_dnd_includes" = ""; then
  offix_dnd_ok=0
  AC_MSG_CHECKING(for Dnd includes)
  offix_dnd_test_include=OffiX/DragAndDrop.h
  for offix_dir in /usr/local/include $offix_this_dir/../DND/DNDlib ; \
  do
    if test -r "$offix_dir/$offix_dnd_test_include"; then
	  offix_dnd_includes=$offix_dir
	  CPPFLAGS="$CPPFLAGS -I$offix_dir"
	  offix_dnd_ok=1
      break
    fi
  done
  if test $offix_dnd_ok -eq 0; then
	 AC_MSG_RESULT(no)
     AC_MSG_ERROR(This package requires Dnd!)
  else
	 AC_MSG_RESULT(yes)
  fi
fi
#find the C libraries
if test "$offix_dnd_libraries" = ""; then
  offix_dnd_ok=0
  AC_MSG_CHECKING(for Dnd libraries)
  for offix_dir in /usr/local/lib $offix_this_dir/../DND/DNDlib ; \
  do
    if test -r "$offix_dir/libDnd.a"; then
	  offix_dnd_libraries=$offix_dir
 	  LDFLAGS="$LDFLAGS -L$offix_dir"
	  offix_dnd_ok=1
      break
    fi
  done
  if test $offix_dnd_ok -eq 0; then
	 AC_MSG_RESULT(no)
     AC_MSG_ERROR(This package requires libDnd (not found)!)
  fi
  offix_dnd_ok=0
#find the C++ libraries
  for offix_dir in $offix_dnd_libraries $offix_this_dir/../DND/DNDlib ; \
  do
    if test -r "$offix_dir/libDnd++.a"; then
	  if test "$offix_dir" != "offix_dnd_libraries"; then	
	    offix_dnd_libraries="$offix_dnd_libraries $offix_dir"
   	    LDFLAGS="$LDFLAGS -L$offix_dir"
	    offix_dnd_ok=1
      	break
	  fi
    fi
  done
  if test $offix_dnd_ok -eq 0; then
  	AC_MSG_RESULT(maybe) 
    AC_MSG_WARN(This package might require libDnd++ (not found)!)
  else
    AC_MSG_RESULT(yes)
  fi 
fi
])
dnl ---- end of OFFIX_CHECK_DND
