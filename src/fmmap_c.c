#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#ifdef POSIX
#include <sys/mman.h>
#define FILEHANDLE int*
#endif

#ifdef WIN32
#include <BaseTsd.h>
#include <WinDef.h>
#include <WinNT.h>
#include <winbase.h>
#define FILEHANDLE HANDLE*
#endif

/*
filemode = 1: scratch file
           2: open an existing file
           3: create a new file
           
error codes:
   1: the file could not be created
   2: the file could be opened
   3: the scratch file could not be unlinked
   4: the scratch file could not be sized
   5: the mapping failed
  
  11: the unmapping failed
  12: the file could not be closed
*/
       
int c_mmap_create( void** ptr           
                 , const long long n    
                 , const int filemode   
                 , const char* filename 
                 , FILEHANDLE fd) {
	
    int stat;
    
#ifdef POSIX
    if (filemode == 1) {
        char path[MAX_PATH];
		if (strlen(filename) == 0) {
            strcpy(path,"./");
        } else {
            strcpy(path,filename);
            strcat(path,"/");
        }
        strcat(path,"fmm.tmpXXXXXX");
        char name[strlen(path)];
        strcpy(name,path);
        *fd = mkstemp(name);       if (*fd <= 0)  return 1;
        stat = unlink(name);       if (stat != 0) return 2;
        stat = ftruncate(*fd, n);  if (stat != 0) return 3;
    } else {
        *fd = open(filename,O_RDWR); if (*fd < 0) return 4;
    }
    // Map the file into memory
    *ptr = mmap ( NULL                        
                , (size_t)n                  
                , PROT_READ | PROT_WRITE      
                , MAP_SHARED | MAP_NORESERVE  
                , *fd                         
                , 0 );
    if (ptr == MAP_FAILED) return 5;
#endif

#ifdef WIN32
	HANDLE hm;
    if (filemode == 1) {
        char path[MAX_PATH], tmpname[MAX_PATH];
		if (strlen(filename) == 0) {
            GetTempPathA(MAX_PATH,path);
        } else {
            strcpy(path,filename);
        }
		GetTempFileNameA(path,"fmm",0u,tmpname);
		char name[strlen(tmpname)];
		strcpy(name,tmpname);
        printf("%s|\n",name);
 		*fd = fopen(name,"wb+");                          if (*fd == NULL) return 1;
		stat = _fseeki64(*fd, (__int64)(n-1), SEEK_SET);  if (stat != 0)   return 2;
		char foo = 0; 
		stat = (int)fwrite(&foo,(size_t)1,(size_t)1,*fd); if (stat == 0)   return 3;
		stat = fclose(*fd);                               if (stat != 0)   return 4;
  		*fd = CreateFileA(name,GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING
                         ,FILE_ATTRIBUTE_NORMAL | FILE_FLAG_DELETE_ON_CLOSE, NULL);
		                                                  if (*fd == NULL) return 5;
	} else {
		*fd = fopen(filename,"rb+");
	}
	hm = CreateFileMappingA(*fd,NULL,PAGE_READWRITE,0,0,NULL); if (hm == NULL)   return 7;
	*ptr = MapViewOfFile(hm,FILE_MAP_ALL_ACCESS,0,0,0);        if (*ptr == NULL) return 8;
#endif

    return 0;
}

int c_mmap_destroy( void* ptr
                  , const long long n
                  , const int filemode   
                  , FILEHANDLE fd) {

    int stat;
    
#ifdef POSIX
    stat = munmap(ptr, (size_t)n); if (stat != 0) return 11;
    stat = close(fd);              if (stat != 0) return 12;
#endif

#ifdef WIN32
    stat = (int)FlushViewOfFile(ptr,0); if (stat == 0) return 11;
    stat = (int)UnmapViewOfFile(ptr);   if (stat == 0) return 12;
	// should we destroy the file mapping ? Probably...
	stat = (int)CloseHandle(fd);        if (stat == 0) return 13;
#endif

	return 0;
}
