#!/usr/bin/gawk -f
#
#########################################################################
#                                                                       #
# Creates the file countryinfo.awk with information from countries	#
# Call gawk -f makecountryinfo.awk countries > countryinfo.awk		#
#                                                                       #
# This is part of my adressbuch / addressbook program                   #
# Version 0.5.6, 02.02.1996                                             #
# Copyright (C) 1995, 1996 Clemens Durka                                #
#                                                                       #
# Clemens Durka (durka@informatik.tu-muenchen.de, clemens@cle.joker.de) #
# Lehrstuhl fuer Effiziente Algorithmen, Prof. Dr. E. Mayr              #
# Technische Universitaet Muenchen                                      #
# Arcisstr. 21                                                          #
# D-80290 Muenchen                                                      #
# Germany                                                               #
#                                                                       #
#########################################################################
#
BEGIN {FS=";"; OFS=" "
print "# Countryinformation"
print "#"
print "### DO NOT EDIT ###"
print "#"
print "# This file has been created automatically from countries"
print "# If you need to change anything, edit countries and contact the author."
print "#"

}

/^#/ { next }

{
print "fc[\"" $1 "\"]=\"" $9 "\""
print "intl_prefix[\"" $1 "\"]=\"" $3 "\""
print "intl_leaveout[\"" $1 "\"]=\"" $5 "\""
}
