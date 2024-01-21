
void pgif_init(char *DBName);
int pgif_search(char *Term);
void pgif_destroy();
char * pgif_GetRecord(int RecordNum, char *ElementSet,
                    char *RecordSyntax, int *ActualLen);



