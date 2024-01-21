SHELL=/bin/sh
MAKE=make

what:
		@echo "what do you want to make?"


##########################################################
#
#  Test iconc.
#

test-iconc:
		make test-opt
		make test-noopt

test-icont:
		sh Test-icont icont
		sh Test-icont large

test-opt:
		sh Test-iconc iconc
		sh Test-iconc invoke -fs
		sh Test-iconc errcnv -fen
		sh Test-iconc large -fdl

test-noopt:
		sh Test-iconc iconc -na -p -w
		sh Test-iconc debug -fde -na -p -w
		sh Test-iconc large -fdl -na -p -w

test-coexpr-iconc:
		sh Test-iconc coexpr -fd
		sh Test-iconc coexpr -fd -na -p -w

test-coexpr-icont:
		sh Test-icont coexpr

test-large:
		sh Test-icont large


##########################################################
#
#  Clean up.
#
Clean:
		rm -f local/*
		touch local/.placeholder


