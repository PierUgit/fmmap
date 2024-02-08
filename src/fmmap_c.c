#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

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
                 , int* fd ) {

    int stat;
    if (filemode == 1) {
        char name[strlen(filename)+6];
        strcpy(name,filename);
        strcat(name,"XXXXXX");
        *fd = mkstemp(name);       if (*fd <= 0)  return 1;
        stat = unlink(name);       if (stat != 0) return 3;
        stat = ftruncate(*fd, n);  if (stat != 0) return 4;
    } else {
        *fd = open(filename,O_RDWR);
        if (*fd < 0) return 2;
    }
    // Map the file into memory
    *ptr = mmap ( NULL                        
                , (size_t)n                  
                , PROT_READ | PROT_WRITE      
                , MAP_SHARED | MAP_NORESERVE  
                , *fd                         
                , 0 );
    if (ptr == MAP_FAILED) return 5;
    return 0;
}

int c_mmap_destroy( void* ptr
                  , const long long n
                  , int fd ) {

    int stat = munmap(ptr, (size_t)n);
    if (stat != 0) return 11;
    stat = close(fd);
    if (stat != 0) return 12;
    return 0;
}