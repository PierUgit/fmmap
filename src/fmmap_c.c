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


// predefined values for the `filestatus` argument
#define FMMAP_LANG_C
#include "constants.h"

typedef struct {
    void*     ptr;
    size_t    n;
    char*     filename;
    int       filestatus;
#ifdef _WIN32
    HANDLE    filedes;
    HANDLE    mapdes;
#else
    int       filedes;
#endif
    bool      private;
} fmmap_t;


       
int c_mmap_create( fmmap_t* x, const char* filename) {
	
    int stat;

//----------------------------------------------------
#ifdef _WIN32
//---------------------WIN32--------------------------
    if (x->filestatus == FMMAP_SCRATCH) {

        // build temp file name
        char path[MAX_PATH], tmpname[MAX_PATH];
        if (strlen(filename) == 0) {
            GetTempPathA(MAX_PATH,path);
        } else {
            strcpy(path,filename);
        }   
        GetTempFileNameA(path,"fmm",0u,tmpname);
        x->filename = malloc(strlen(tmpname)+1);
        strcpy( x->filename, tmpname );

        // create temp file
        x->filedes = fopen(tmpname,"wb+");                          
            if (x->filedes == NULL) return 101;
        stat = _fseeki64(x->filedes, (__int64)(x->n-1), SEEK_SET);
            if (stat != 0)   return 101;
        char foo = 0; 
        size_t statwrt = fwrite(&foo,(size_t)1,(size_t)1,x->filedes);
            if (statwrt != 1)   return 101;
        stat = fclose(x->filedes);                               
            if (stat != 0)   return 101;

        // (re)open temp file with the Windows API
        x->filedes = CreateFileA( tmpname
                                , GENERIC_READ | GENERIC_WRITE
                                , 0
                                , NULL
                                , OPEN_EXISTING
                                , FILE_ATTRIBUTE_NORMAL | FILE_FLAG_DELETE_ON_CLOSE
                                , NULL );
        if (x->filedes == INVALID_HANDLE_VALUE) return 111;
	    
    } else if (x->filestatus == FMMAP_NOFILE) {
    
        x->filedes = INVALID_HANDLE_VALUE;
        
    } else {
	    
	    x->filename = malloc(strlen(filename)+1);
	    strcpy( x->filename, filename );

	    // open file
	    x->filedes = CreateFileA( filename
                                , GENERIC_READ | GENERIC_WRITE
                                , FILE_SHARE_READ | FILE_SHARE_WRITE
                                , NULL
                                , OPEN_EXISTING
                                , FILE_ATTRIBUTE_NORMAL
                                , NULL );
	    if (x->filedes == INVALID_HANDLE_VALUE) return 111;
	    
	}

    // map file and define a view
    DWORD nhi = (x->n >> 32);
    DWORD nlo = x->n - ((size_t)nhi << 32);
    x->mapdes = CreateFileMappingA(x->filedes,NULL,PAGE_READWRITE,nhi,nlo,NULL); 
        if (x->mapdes == NULL)   return 121;
    x->ptr = MapViewOfFile( x->mapdes
                          , (x->private ? FILE_MAP_COPY : FILE_MAP_ALL_ACCESS)
                          , 0, 0, x->n );        
    if (x->ptr == NULL) return 122;
    
    // close the file
    if (x->filestatus != FMMAP_NOFILE) {
        stat = (int)CloseHandle(x->filedes);
            if (stat == 0) return 131;
    }
//----------------------------------------------------
#else
//---------------------POSIX--------------------------
    if (x->filestatus == FMMAP_SCRATCH) {

        // build temp file name
        char tmpname[1024];
        if (strlen(filename) == 0) {
            strcpy(tmpname,"./");
        } else {
            strcpy(tmpname,filename);
            strcat(tmpname,"/");
        }
        strcat(tmpname,"fmm.tmpXXXXXX");
        x->filename = malloc(strlen(tmpname)+1);
        strcpy( x->filename, tmpname );

        // create temp file (and keep it open)
        x->filedes = mkstemp(tmpname);
            if (x->filedes <= 0)  return 101;
        stat = unlink(tmpname);   // without unlink, the file is not removed at the end on macOS (bug)
            if (stat != 0) return 101;
        stat = ftruncate(x->filedes, x->n);
            if (stat != 0) return 101;
	
	} else if (x->filestatus == FMMAP_NOFILE) {
	
	    x->filedes = -1;
    
    } else {
	    
        x->filename = malloc(strlen(filename)+1);
        strcpy( x->filename, filename );

        // open file
        x->filedes = open(filename,O_RDWR);
            if (x->filedes < 0) return 111;
	    
    }
	
    // Map the file into memory
    x->ptr = mmap ( NULL                        
                  , (size_t)x->n                  
                  , PROT_READ | PROT_WRITE      
                  ,   (x->private ? MAP_PRIVATE : MAP_SHARED) 
                    | (x->filestatus == FMMAP_NOFILE ? MAP_ANONYMOUS : 0) 
                    | MAP_NORESERVE  
                  , x->filedes                         
                  , 0 );
    if (x->ptr == MAP_FAILED) return 121;
    
    // close the file
    if (x->filestatus != FMMAP_NOFILE) {
        stat = close(x->filedes);
            if (stat != 0) return 131;
    } 
//----------------------------------------------------
#endif
//----------------------------------------------------

    return 0;
}



int c_mmap_destroy( fmmap_t* x, const bool wb ) {

    int stat;
    size_t statwrt;
    
//----------------------------------------------------
#ifdef _WIN32
//---------------------WIN32--------------------------
    if (wb) {

    // writeback case, create another view on the same mapping
    // and copy one view to the other
        void* ptr2 = MapViewOfFile( x->mapdes
                                 , FILE_MAP_ALL_ACCESS
                                 , 0, 0, x->n);
        if (ptr2 == NULL) return 201;
        memcpy(ptr2,x->ptr,(size_t)x->n);
        stat = (int)UnmapViewOfFile(ptr2);
            if (stat == 0) return 202;
	    
    }
    if (x->filestatus == FMMAP_NEW || x->filestatus == FMMAP_OLD) {
        // flushing for the NEW and OLD cases
        stat = (int)FlushViewOfFile(x->ptr,0);   // not sure it's needed
            if (stat == 0) return 211;
    }
    // closing the view and the mapping
    stat = (int)UnmapViewOfFile(x->ptr);
        if (stat == 0) return 222;
    stat = (int)CloseHandle(x->mapdes);
        if (stat == 0) return 221;
//----------------------------------------------------
#else
//---------------------POSIX--------------------------
    if (wb) {
        // writeback case, new mapping in shared mode
        fmmap_t y;
        y.n = x->n;
        y.filestatus = FMMAP_OLD;
        y.private = false;
        stat = c_mmap_create( &y, x->filename );
            if (stat != 0) return 201;
        memcpy(y.ptr,x->ptr,x->n); 
        stat = c_mmap_destroy( &y, false );
            if (stat != 0) return 202;
    }
    if (x->filestatus == FMMAP_NEW || x->filestatus == FMMAP_OLD) {
        // flushing for the NEW and OLD cases
        stat = msync(x->ptr, (size_t)x->n, MS_SYNC);
        if (stat != 0) return 211;
    }
    // unmapping the file
    stat = munmap(x->ptr, (size_t)x->n);
        if (stat != 0) return 221;
//----------------------------------------------------
#endif
//----------------------------------------------------

    if (x->filestatus != FMMAP_NOFILE) free( x->filename );

    return 0;
}

