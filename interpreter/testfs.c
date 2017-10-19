
// test file to check the direcotory and filesystem sys calls
/**************************************************************************************************/
#include "syscall.h"
#include "directory.c"
#include "syscallstubs.c"
#include "debug.c"
#include <stdlib.h>
#include <stdint.h>
/**************************************************************************************************/
void print_target_info(void) {
  int ptr_size = PTR_SZ;
  printf("native pointer size:%d\n", ptr_size);
  int int_size = INT_SZ;
  printf("native int size:%d\n", int_size);
}
/**************************************************************************************************/
int main(int argc, char** argv) {
  print_target_info();
  init_system();
  dump_all_dirs();
  shut_down();
  return 123;
}
/**************************************************************************************************/

