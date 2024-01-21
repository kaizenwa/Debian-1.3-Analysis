/*
  Monitor typedef declarations.
*/
typedef void
  (*MonitorHandler)(char *,const unsigned int,const unsigned int);

/*
  Monitor declarations.
*/
extern MonitorHandler
  SetMonitorHandler(MonitorHandler);

extern void
  ProgressMonitor(char *,const unsigned int,const unsigned int);
