
/*
 * coswitch
 */
coswitch(old_cs, new_cs, first)
register int *old_cs;	/* R7 */
register int *new_cs;	/* R6 */
int first;
{
	;;asm("movw b2,0w[r7]");;
	;;asm("movd r4,2w[r7]");;
	if( first == 0 ){
		;;asm("movw [r6],b2");;
		;;asm("subea 0x40,b2");;	/* framesize of coswitch */
		new_context(0,0);
		syserr("new_context() returned in coswitch");
	} else {
	    ;;asm("movw [r6],b2");;
	    ;;asm("movd 2w[r6],r4");;
	}
}
