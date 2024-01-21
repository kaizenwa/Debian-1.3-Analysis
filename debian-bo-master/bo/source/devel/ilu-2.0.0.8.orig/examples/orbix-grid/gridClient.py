import ilu, sys, grid

def do_test (grid_ior):
	g = ilu.ObjectOfSBH(grid.grid, grid_ior)
	width = g._get_width()
	height = g._get_height()
	print 'width = %d, height = %d' % (width, height)
	g.set(1, 1, 23)
	val = g.get (1, 1)
	if (val != 23):
		print "wrong val", val, "returned by get(1,1)"
	else:
		print "set of (1,1) succeeded"

def main (argv):
	if not (len(argv) == 2):
		print 'Usage:  %s GRID-IOR'
		sys.exit(1)
	do_test(argv[1])

if __name__ == '__main__':
	main(sys.argv)
		
