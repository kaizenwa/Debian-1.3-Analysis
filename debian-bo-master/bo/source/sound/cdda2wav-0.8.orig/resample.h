extern long waitforsignal;	/* flag: wait for any audio response */

extern short undersampling;	/* conversion factor */
extern short samples_to_do;	/* loop variable for conversion */
extern int Halved;		/* interpolate due to non integral divider */
extern int jitterShift;		/* track accumulated jitter */
long SaveBuffer (unsigned char *p, long channels, 
                 unsigned long SecsToDo, unsigned long *BytesDone, 
		 unsigned long TotalSecs);
