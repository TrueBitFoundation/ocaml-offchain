
/***********************************************************************************************/
#ifndef DEBUG_C
#define DEBUG_C
/***********************************************************************************************/
#include "syscall.h"
#include "directory.c"
#include "syscallstubs.c"
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
/***********************************************************************************************/
void dump_all_dirs(void) {
  struct dir* current = dir_head;
  while(current != NULL) {
    printf("dir : %d : ", current->dirfd);
    if (current->dir_name) printf("%s\n", current->dir_name);
    current = current->next;
  }
}
/***********************************************************************************************/
#endif
/***********************************************************************************************/

