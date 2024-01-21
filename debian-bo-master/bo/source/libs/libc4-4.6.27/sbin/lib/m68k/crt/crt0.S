	.text
__entry:
	movel sp@(8),d0
	movel d0,___environ
	jbsr  _main
/*
 *	We use trap #0 for __exit().
 */
	movel d0,d1
done:
	moveq #1,d0
	trap  #0
	bras  done
