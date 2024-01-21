# This script sets up the necessary environment to build XView for Linux.
# It should be sourced by bash.
# pmake is required for compilation (unless you're _really_ lucky and XView
# builds with your version of GNU-make).

if ! [ -d lib/libxview ]; then
  echo Please change to XView source tree and try again
  exit 1
fi

rm -f make
ln -s /usr/bin/pmake make

# I need a special setup for my machine, because I haven't completely
# converted to ELF yet
if [ -d /usr/elf ]; then
  [ "$XVIEW_SETUP" ] || PATH="/usr/elf/bin:/usr/X11R6/bin:$PATH"
  IMAKEINCLUDE="-I`pwd`/config -I/usr/X11R6-3.1.2/lib/X11/config"
  ELFPREFIX=/usr/elf
else
  IMAKEINCLUDE="-I`pwd`/config -I/usr/X11R6/lib/X11/config"
  ELFPREFIX=/usr
fi

[ "$XVIEW_SETUP" ] || PATH="`pwd`:$PATH"
PS1='\h:\w(XView-build)\$ '
XVIEW_SETUP=1
export PATH PS1 IMAKEINCLUDE XVIEW_SETUP
hash -r


if [ "x$1" = xall ]; then
  set -- libs instlibs clients instclients instfinish
fi

while [ $# -gt 0 ]; do
  case "$1" in
    libs)
      cd config
      imake -DUseInstalled $IMAKEINCLUDE
      cd ..
      imake -DUseInstalled $IMAKEINCLUDE
      make World
      ;;
    instlibs)
      make install install.man
      make SUBDIRS=doc install
      ;;
    clients)
      make Clients Contrib
      (
        cd clients/olvwm-4.1
        imake -DUseInstalled $IMAKEINCLUDE
        make depend BSDCCFLAGS="-I$ELFPREFIX/include/bsd -include $ELFPREFIX/include/bsd/bsd.h"
        make BSDCCFLAGS="-I$ELFPREFIX/include/bsd -include $ELFPREFIX/include/bsd/bsd.h"
      )
      ;;
    instclients)
      make 'SUBDIRS=clients contrib' install install.man install.srcs
      cd clients/olvwm-4.1 && make install install.man
      ;;
    instfinish)
      [ -e /usr/openwin/lib/openwin-menu-std ] || mv /usr/openwin/lib/openwin-menu /usr/openwin/lib/openwin-menu-std
      cp contrib/misc/{openwin-menu*,Xinitrc} /usr/openwin/lib
      cp contrib/misc/{openwin,owplaces} /usr/openwin/bin
      mkdir /usr/openwin/bin/sunview
      mv /usr/openwin/bin/{cv2*,*.sed} /usr/openwin/bin/sunview
      chown -R root.root /usr/openwin /usr/X11R6/lib/X11/config/XView.*
      chmod -R a+rX,u+w,go-w /usr/openwin /usr/X11R6/lib/X11/config/XView.*
      chmod a+x /usr/openwin/share/src/xview/examples/bin/* /usr/openwin/bin/sunview/*
      ;;
    tar)
      DIRNAME="`pwd`"
      DIRNAME="${DIRNAME##*/}"
      tar cv /usr/openwin /usr/X11R6/lib/X11/config/XView.* | gzip -9 > ../$DIRNAME.bin.tar.gz
      ;;
    srctar)
      rm -f make
      DIRNAME="`pwd`"
      DIRNAME="${DIRNAME##*/}"
      (
        cd ..
        chmod -R a+rX,u+w,go-w "$DIRNAME"
        tar cv "$DIRNAME" | gzip -9 > $DIRNAME.src.tar.gz
      )
      ;;
    clean)
      [ -e clients/olvwm-4.1/Makefile ] && (cd clients/olvwm-4.1 && make clean)
      [ -e Makefile ] && make Clean
      find -name Makefile -o -name "*~" -o -name "*.so.*" | xargs -r rm
      rm -f make
      ;;
    diff)
      rm -f make
      DIRNAME="`pwd`"
      DIRNAME="${DIRNAME##*/}"
      (
        cd ..
        diff --context --recursive --new-file xview3.2p1-X11R6 $DIRNAME | gzip -9 > $DIRNAME.diff.gz
      )
      ;;
  esac
  shift
done
