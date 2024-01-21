/*
 * errmsg.r -- err_msg, irunerr, drunerr
 */

hidden novalue err_txt Params((int n));

#ifdef PresentationManager
extern MRESULT_N_EXPENTRY RuntimeErrorDlgProc(HWND, ULONG, MPARAM, MPARAM);
HAB HInterpAnchorBlock;
#endif					/* PresentationManager */

extern struct errtab errtab[];		/* error numbers and messages */

/*
 * err_msg - print run-time error message, performing trace back if required.
 *  This function underlies the rtt runerr() construct.
 */
novalue err_msg(n, v)
int n;
dptr v;
{
   register struct errtab *p;
#ifdef PresentationManager
   HMODULE modhand;
#endif

   if (n == 0) {
      k_errornumber = t_errornumber;
      k_errorvalue = t_errorvalue;
      have_errval = t_have_val;
      }
   else {
      k_errornumber = n;
      if (v == NULL) {
         k_errorvalue = nulldesc;
         have_errval = 0;
         }
      else {
         k_errorvalue = *v;
         have_errval = 1;
         }
      }

   k_errortext = "";
   for (p = errtab; p->err_no > 0; p++)
      if (p->err_no == k_errornumber) {
         k_errortext = p->errmsg;
         break;
         }

   EVVal((word)k_errornumber,E_Error);

#ifndef PresentationManager
   if (pfp != NULL) {
      if (IntVal(kywd_err) == 0 || !err_conv) {
         fprintf(stderr, "\nRun-time error %d\n", k_errornumber);
#if COMPILER
         if (line_info)
            fprintf(stderr, "File %s; Line %d\n", file_name, line_num);
#else					/* COMPILER */
         fprintf(stderr, "File %s; Line %ld\n", findfile(ipc.opnd),
            (long)findline(ipc.opnd));
#endif					/* COMPILER */
         }
      else {
         IntVal(kywd_err)--;
         return;
         }
      }
   else
      fprintf(stderr, "\nRun-time error %d in startup code\n", n);
   fprintf(stderr, "%s\n", k_errortext);

   if (have_errval) {
      fprintf(stderr, "offending value: ");
      outimage(stderr, &k_errorvalue, 0);
      putc('\n', stderr);
      }

   if (!debug_info)
      c_exit(ErrorExit);

   if (pfp == NULL) {		/* skip if start-up problem */
      if (dodump)
         abort();
      c_exit(ErrorExit);
      }

   fprintf(stderr, "Trace back:\n");
   tracebk(pfp, argp);
   fflush(stderr);


   if (dodump)
      abort();

   c_exit(ErrorExit);
#else					/* PresentationManager */

  if (pfp != NULL) {
     if (IntVal(kywd_err) == 0 || !err_conv) {
	 DosQueryModuleHandle("xiconxdl.dll",&modhand);
	 if (WinDlgBox(HWND_DESKTOP, HWND_DESKTOP, RuntimeErrorDlgProc, modhand,
		IDD_RUNERR, NULL) == DID_ERROR) {

	  WinMessageBox(HWND_DESKTOP, HWND_DESKTOP,
		  "An Error occurred, but the dialog cannot be loaded.\nExecution halting.",
		  "Icon Runtime System", 0, MB_OK|MB_ICONHAND|MB_MOVEABLE);
	 }
     }
     else {
	IntVal(kywd_err)--;
	return;
     }
  }

  if (dodump)
    abort();

  c_exit(ErrorExit);
#endif					/* PresentationManager */
}

/*
 * irunerr - print an error message when the offending value is a C_integer
 *  rather than a descriptor.
 */
novalue irunerr(n, v)
int n;
C_integer v;
   {
   t_errornumber = n;
   IntVal(t_errorvalue) = v;
   t_errorvalue.dword = D_Integer;
   t_have_val = 1;
   err_msg(0,NULL);
   }

/*
 * drunerr - print an error message when the offending value is a C double
 *  rather than a descriptor.
 */
novalue drunerr(n, v)
int n;
double v;
   {
   union block *bp;

   bp = (union block *)alcreal(v);
   if (bp != NULL) {
      t_errornumber = n;
      BlkLoc(t_errorvalue) = bp;
      t_errorvalue.dword = D_Real;
      t_have_val = 1;
      }
   err_msg(0,NULL);
   }
