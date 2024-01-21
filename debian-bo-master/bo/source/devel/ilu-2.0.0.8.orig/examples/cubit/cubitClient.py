import sys
import ilu
import cubit
import traceback

def trunc (val, limit):
	if (val < 0): sign, val = -1, -val
	else: sign = 1
	return sign * (val % limit)

def do_tests (handle):

	# cube byte

	try:
		val = trunc(13, 0x100)
		retval = handle.cube_octet(val)
		print "cube octet:  %d --> %d" % (val, retval)
	except:
		print "exception from cube_octet(%s)" % val
		traceback.print_exc()

	# cube short

	try:
		val = trunc(-117, 0x10000)
		retval = handle.cube_short(val)
		print "cube short:  %d --> %d" % (val, retval)
	except:
		print "exception from cube_short(%s)" % val
		traceback.print_exc()

	# cube long

	try:
		val = trunc(-117, 0x100000000L)
		retval = handle.cube_long(val)
		print "cube long:  %d --> %d" % (val, retval)
	except:
		print "exception from cube_long(%s):" % val
		traceback.print_exc()

	# cube struct

	try:
		origval = {'o' : trunc(13, 0x100), 's' : trunc(-117, 0x10000), 'l' : trunc(-117, 0x100000000L) }
		retval = handle.cube_struct(origval)
		print "cube struct:  %s --> %s" % (origval, retval)
	except:
		print "exception from cube_struct(%s):" % origval
		traceback.print_exc()

def main(sbh):

	handle = ilu.ObjectOfSBH (cubit.Cubit, sbh)
	if not handle:
		print "Can't parse URL", sbh
		sys.exit(2)
	do_tests (handle)

if __name__ == '__main__':
	if len(sys.argv) < 2:
		print "Usage:  %s OBJECT-SBH" % sys.argv[0]
		sys.exit(1)
	else:
		main (sys.argv[1])

