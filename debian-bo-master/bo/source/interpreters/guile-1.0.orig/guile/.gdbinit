dir ../libguile
dir ../gtcltk-lib

# The command below assumes that you've installed Mikael Djurfeldt's
# wonderful patches to GDB, that allow GDB to print and parse Guile
# Scheme values directly.
#
# "I don't know where I'd be without them.  Maybe Cleveland."
#		-- Jim Blandy, Guile maintainer
set print lisp

# This is like setting a breakpoint on abort or exit --- it gives us a
# chance to see the state of the program just before it exits.
break uncaught_throw
