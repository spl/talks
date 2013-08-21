
#ifdef DO_PAUSES

#define PAUSE           \pause
#define INCREMENT       [<+->]
#define ONSLIDE(N)      \onslide<N->

#else

#define PAUSE
#define INCREMENT
#define ONSLIDE(N)

#endif

#define LINE            \(\)
#define PAUSE_LINE      PAUSE LINE

