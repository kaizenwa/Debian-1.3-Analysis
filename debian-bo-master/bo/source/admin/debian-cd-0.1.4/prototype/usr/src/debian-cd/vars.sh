# User-set variables for the CD information.

# PLEASE FILL IN YOUR NAME HERE.
#
echo "WARNING: If your name isn't Bruce Perens, please edit vars.sh ." 2>&1

PREPARER="Bruce Perens <bruce@debian.org>"

# Where your mirror of the Debian FTP archive is.
ARCHIVE=/debian/CD/debian

# Where you want the CD images to be stored.
# If you want them to be stored in the current directory, set TARGET=.
TARGET=/mnt

# Paths of files that should _NEVER_ be on your CD.
ALWAYS_EXCLUDE="$ARCHIVE/non-free"

# The code name of the present Debian release.
CODE_NAME="bo"

# Where the release lives. Calculated from the above.
RELEASE=$ARCHIVE/$CODE_NAME

# The architecture of the CD you are producing.
ARCHITECTURE=i386

#        APPI   The  application  identifier  should  describe  the
#               application that will be on  the  disc.   There  is
#               space  on  the  disc for 128 characters of informa­
#               tion.  May be overridden using the -A command  line
#               option.

APPI="Debian GNU/Linux 1.3"

#        COPY   The copyright information, often the name of a file
#               on the disc containing the copyright notice.  There
#               is  space in the disc for 37 characters of informa
#               tion.
 
COPY="Copyrighted by SPI and others."

#        ABST   The abstract information, often the name of a  file
#               on the disc containing an abstract.  There is space
#               in the disc for 37 characters of information.

# ABST="readme.txt"
 
#        BIBL   The bibliographic information, often the name of  a
#               file  on the disc containing a bibliography.  There
#               is space in the disc for 37 characters of  informa-
#               tion.

# BIBL=""

#        PREP   This  should  describe  the  preparer of the CDROM,
#               usually with a mailing address  and  phone  number.
#               There  is  space  on the disc for 128 characters of
#               information.  May be overridden using the  -p  com-
#               mand line option.
 
# PREP="$PREPARER"

#        PUBL   This  should  describe  the publisher of the CDROM,
#               usually with a mailing address  and  phone  number.
#               There  is  space  on the disc for 128 characters of
#               information.  May be overridden using the  -P  com-
#               mand line option.

PUBL="$PREPARER"

#        SYSI   The  System Identifier.  There is space on the disc
#               for 32 characters of information.

SYSI="Linux"

#        VOLI   The Volume Identifier.  There is space on the  disc
#               for  32 characters of information.  May be overrid-
#               den using the -V command line option.
 
# VOLI=""

#        VOLS   The Volume Set Name.  There is space  on  the  disc
#               for 278 characters of information.

VOLS="Debian GNU/Linux 1.3 . A distribution of a free-software Linux system.
for information, please see http://www.debian.org/ .
`date`"
