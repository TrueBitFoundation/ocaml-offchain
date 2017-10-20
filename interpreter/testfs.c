
// test file to check the direcotory and filesystem sys calls
/***********************************************************************************************/
#include "syscall.h"
#include "directory.h"
#include "syscallstubs.c"
#include "debug.c"
#include <stdlib.h>
#include <stdint.h>
/***********************************************************************************************/
void print_target_info(void) {
  int ptr_size = PTR_SZ;
  printf("native pointer size:%d\n", ptr_size);
  int int_size = INT_SZ;
  printf("native int size:%d\n", int_size);
  printf("cwd : %s\n", fs_cache.cwd->dir_name);
  printf("dir_head: %s\n", dir_head->dir_name);
  printf("dir_tail: %s\n", dir_tail->dir_name);
}

enum direction_t upordown(const char* path) {
  enum direction_t direction = 2;
  if (path[0] == '.') {
    if (path[1] == '.') direction = up;
    else direction = down;
  }
  return direction;
}
/***********************************************************************************************/
int main(int argc, char** argv) {
  enum direction_t direction;
  init_system();
  print_target_info();
  int which = 123;
  const char* dir1 = "/usr/bin/local";
  const char* dir2 = "./loco/inu";
  const char* dir3 = "../../../hehe/hahah/mwahahah";
  printf("%d",IS_RELATIVE(dir1));
  printf("%d",IS_RELATIVE(dir2));
  printf("%d\n",IS_RELATIVE(dir3));
  int* varargs;
  char* dsp1 = dissect_path(dir1);
  printf("direction: %d\n", upordown(dsp1));
  char* dsp2 = dissect_path(dir2);
  printf("direction: %d\n", upordown(dsp2));
  char* dsp3 = dissect_path(dir3);
  printf("direction: %d\n", upordown(dsp3));
  //char* dsp4 = read_dissected_path(dsp3, 3);
  //printf("dsp: %s\n", dsp4);
  illuminate_path(fs_cache.cwd, dsp2);
  illuminate_path(fs_cache.cwd, dsp3);
  //env____syscall39(which, (int*)dir1);

  dump_all_dirs();
  shut_down();
  return 123;
}
/***********************************************************************************************/

