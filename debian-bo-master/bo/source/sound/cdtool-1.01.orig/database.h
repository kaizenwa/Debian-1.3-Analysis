/* database.h */

typedef struct {
    char track_names[100][100];
    char cdname[100];
    char artist[100];
} cd_t;

cd_t *read_db(char *tracks_buffer, char usedb);
