# Rules file for Wireworld
#
# More on this automaton may be found in the January 1990 issue of
# Scientific American (Computer Recreations, p. 146).

# 0 is space, 1 is wire, 2 is tail, 3 is head
states 4

passive 2
0[0123][0123][0123][0123]0	# No way to make a space into signal or wire

# Signal propagation
2[0123][0123][0123][0123]1	# tail -> wire
3[0123][0123][0123][0123]2	# head -> tail

# 1 or 2 heads adjacent to a wire makes it a head
1(3*1)3			# wire with 1 head adjacent -> head
1(3*2)3			# wire with 2 heads adjacent -> head

# End of ruleset
