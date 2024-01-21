/* Prototypes */
int InitRaw ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes);
int ExitRaw ( int audio, unsigned long nBytesDone );
unsigned long GetRawHdrSize( void );


