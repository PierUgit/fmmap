#ifdef FMMAP_LANG_F
#define DECL() integer, parameter ::
#else
#define DECL() const int
#endif
   DECL() FMMAP_SCRATCH = 1;
   DECL() FMMAP_OLD     = 2;
   DECL() FMMAP_NEW     = 3;
   DECL() FMMAP_NOFILE  = 4;
