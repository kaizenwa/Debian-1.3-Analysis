int seminstall(key_t key, int amount);
int semrequest(int semid, int semnum);
int semrelease(int semid, int semnum);
int shm_request(key_t key, unsigned int size, unsigned char **memptr);
