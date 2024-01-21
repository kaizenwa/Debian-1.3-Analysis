#if 0
#pragma weak __environ
#endif
#pragma weak environ = __environ

char **__environ = 0;
