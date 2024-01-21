unsigned tracks;

int GetTrack( unsigned long sector );
long FirstTrack ( void );
long GetEndSector ( unsigned long p_track );
long GetStartSector ( unsigned long p_track );
long GetLastSectorOnCd( unsigned long p_track );
void DisplayToc ( void );
void Read_MCN_ISRC( void );
unsigned ScanIndices( unsigned trackval, unsigned indexval );
