void driver8_setpixel(int, int, int);
int driver8_getpixel(int, int);
void driver8_hline(int, int, int, int);
void driver8_fillbox(int, int, int, int, int);
void driver8_putbox(int, int, int, int, void *, int);
void driver8_getbox(int, int, int, int, void *, int);
void driver8_putboxmask(int, int, int, int, void *);
void driver8_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver8_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver8_copybox(int, int, int, int, int, int);

void driver16_setpixel(int, int, int);
int driver16_getpixel(int, int);
void driver16_hline(int, int, int, int);
void driver16_fillbox(int, int, int, int, int);
void driver16_putbox(int, int, int, int, void *, int);
void driver16_getbox(int, int, int, int, void *, int);
void driver16_putboxmask(int, int, int, int, void *);
void driver16_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver16_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver16_copybox(int, int, int, int, int, int);

void driver24_setpixel(int, int, int);
int driver24_getpixel(int, int);
void driver24_hline(int, int, int, int);
void driver24_fillbox(int, int, int, int, int);
void driver24_putbox(int, int, int, int, void *, int);
void driver24_getbox(int, int, int, int, void *, int);
void driver24_putboxmask(int, int, int, int, void *);
void driver24_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver24_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver24_copybox(int, int, int, int, int, int);
void driver24_putbox32(int, int, int, int, void *, int);

void driver32_setpixel(int, int, int);
int driver32_getpixel(int, int);
void driver32_hline(int, int, int, int);
void driver32_fillbox(int, int, int, int, int);
void driver32_putbox(int, int, int, int, void *, int);
void driver32_getbox(int, int, int, int, void *, int);
void driver32_putboxmask(int, int, int, int, void *);
void driver32_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver32_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver32_copybox(int, int, int, int, int, int);

void driver8p_setpixel(int, int, int);
int driver8p_getpixel(int, int);
void driver8p_hline(int, int, int, int);
void driver8p_fillbox(int, int, int, int, int);
void driver8p_putbox(int, int, int, int, void *, int);
void driver8p_getbox(int, int, int, int, void *, int);
void driver8p_putboxmask(int, int, int, int, void *);
void driver8p_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver8p_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver8p_copybox(int, int, int, int, int, int);

void driver16p_setpixel(int, int, int);
int driver16p_getpixel(int, int);
void driver16p_hline(int, int, int, int);
void driver16p_fillbox(int, int, int, int, int);
void driver16p_putbox(int, int, int, int, void *, int);
void driver16p_getbox(int, int, int, int, void *, int);
void driver16p_putboxmask(int, int, int, int, void *);
void driver16p_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver16p_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver16p_copybox(int, int, int, int, int, int);

void driver24p_setpixel(int, int, int);
int driver24p_getpixel(int, int);
void driver24p_hline(int, int, int, int);
void driver24p_fillbox(int, int, int, int, int);
void driver24p_putbox(int, int, int, int, void *, int);
void driver24p_getbox(int, int, int, int, void *, int);
void driver24p_putboxmask(int, int, int, int, void *);
void driver24p_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver24p_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver24p_copybox(int, int, int, int, int, int);

void driver32p_setpixel(int, int, int);
int driver32p_getpixel(int, int);
void driver32p_hline(int, int, int, int);
void driver32p_fillbox(int, int, int, int, int);
void driver32p_putbox(int, int, int, int, void *, int);
void driver32p_getbox(int, int, int, int, void *, int);
void driver32p_putboxmask(int, int, int, int, void *);
void driver32p_putboxpart(int, int, int, int, int, int, void *, int, int);
void driver32p_getboxpart(int, int, int, int, int, int, void *, int, int);
void driver32p_copybox(int, int, int, int, int, int);

void driver8a_fillbox(int, int, int, int, int);
void driver8a_copybox(int, int, int, int, int, int);

void driverplanar256_nothing(void);
void driverplanar256_putbox(int, int, int, int, void *, int);

void driverplanar16_nothing(void);

/* Generic functions */

int driver_setread(GraphicsContext * gc, int i, void **vp);
int driver_setwrite(GraphicsContext * gc, int i, void **vp);
