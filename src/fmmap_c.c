#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#ifdef _WIN32
#include <BaseTsd.h>
#include <WinDef.h>
#include <WinNT.h>
#include <winbase.h>
#else
// posix assumed
#include <sys/mman.h>
#endif

/*
filemode = 1: scratch file
           2: open an existing file
           3: create a new file
           
error codes:
    (later)
*/


typedef struct {
    void*     ptr;
    long long n;
    int       filemode;
#ifdef _WIN32
    HANDLE    filedes;
    HANDLE    mapdes;
#else
    int       filedes;
#endif
    bool      cow;
    bool      used;
} fmmap_t;


       
int c_mmap_create( fmmap_t* x, const char* filename) {
	
    int stat;
    
#ifdef _WIN32
    if (x->filemode == 1) {
        char path[MAX_PATH], tmpname[MAX_PATH];
		if (strlen(filename) == 0) {
            GetTempPathA(MAX_PATH,path);
        } else {
            strcpy(path,filename);
        }
		GetTempFileNameA(path,"fmm",0u,tmpname);
 		x->filedes = fopen(tmpname,"wb+");                          
            if (x->filedes == NULL) return 1;
		stat = _fseeki64(x->filedes, (__int64)(x->n-1), SEEK_SET);
            if (stat != 0)   return 2;
		char foo = 0; 
		size_t statwrt = fwrite(&foo,(size_t)1,(size_t)1,x->filedes);
            if (statwrt != 1)   return 3;
		stat = fclose(x->filedes);                               
            if (stat != 0)   return 4;
  		x->filedes = CreateFileA(tmpname,GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING
                                ,FILE_ATTRIBUTE_NORMAL | FILE_FLAG_DELETE_ON_CLOSE, NULL);
		    if (x->filedes == INVALID_HANDLE_VALUE) return 5;
    } else {
		x->filedes = CreateFileA(filename,GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING
                                ,FILE_ATTRIBUTE_NORMAL, NULL);
		if (x->filedes == INVALID_HANDLE_VALUE) return 7;
	}
	x->mapdes = CreateFileMappingA(x->filedes,NULL,PAGE_READWRITE,0,0,NULL); 
        if (x->mapdes == NULL)   return 8;
	x->ptr = MapViewOfFile( x->mapdes
	                      , (x->cow ? FILE_MAP_COPY : FILE_MAP_ALL_ACCESS)
	                      , 0, 0, 0);        
    if (x->ptr == NULL) return 9;
#else
    if (x->filemode == 1) {
        char tmpname[1024];
		if (strlen(filename) == 0) {
            strcpy(tmpname,"./");
        } else {
            strcpy(tmpname,filename);
            strcat(tmpname,"/");
        }
        strcat(tmpname,"fmm.tmpXXXXXX");
        x->filedes = mkstemp(tmpname);
            if (x->filedes <= 0)  return 1;
        stat = unlink(tmpname);
            if (stat != 0) return 2;
        stat = ftruncate(x->filedes, x->n);
            if (stat != 0) return 3;
    } else {
        x->filedes = open(filename,O_RDWR);
            if (x->filedes < 0) return 4;
    }
    // Map the file into memory
    x->ptr = mmap ( NULL                        
                  , (size_t)x->n                  
                  , PROT_READ | PROT_WRITE      
                  , (x->cow ? MAP_PRIVATE : MAP_SHARED) | MAP_NORESERVE  
                  , x->filedes                         
                  , 0 );
    if (x->ptr == MAP_FAILED) return 5;
#endif

    return 0;
}



int c_mmap_destroy( fmmap_t* x, const bool wb ) {

    int stat;
    size_t statwrt;
    
#ifdef _WIN32
    if (wb) {
        void* ptr2 = MapViewOfFile( x->mapdes
	                             , FILE_MAP_ALL_ACCESS
	                             , 0, 0, 0);
        if (ptr2 == NULL) return 15;
        memcpy(ptr2,x->ptr,(size_t)x->n);
        stat = (int)UnmapViewOfFile(ptr2);
            if (stat == 0) return 16;
    }
    if (x->filemode != 1) {
        stat = (int)FlushViewOfFile(x->ptr,0);   // not sure it's needed
            if (stat == 0) return 11;
    }
    stat = (int)UnmapViewOfFile(x->ptr);
        if (stat == 0) return 12;
    stat = (int)CloseHandle(x->mapdes);
        if (stat == 0) return 13;
	stat = (int)CloseHandle(x->filedes);
	    if (stat == 0) return 14;
#else
    if (wb) {
        statwrt = write(x->filedes, x->ptr, (size_t)x->n);
            if (statwrt != (size_t)x->n) return 15;
        stat = fsync(x->filedes);   // not sure it's needed...
            if (stat != 0) return 16; 
    }
    if (x->filemode != 1) {
        stat = msync(x->ptr, (size_t)x->n, MS_SYNC);
        if (stat != 0) return 11;
    }
    stat = munmap(x->ptr, (size_t)x->n);
        if (stat != 0) return 12;
    stat = close(x->filedes);
        if (stat != 0) return 14;
#endif

	return 0;
}

