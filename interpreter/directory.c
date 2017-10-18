
#ifndef DIRECTORY_C
#define DIRECTORY_C
#include "syscall.h"
#include <stdlib.h>
#include <stdint.h>

#define mode_t int

struct linux_dirent {
  unsigned long d_ino;
  unsigned long d_off;
  unsigned short d_reclen;
  char* dname;
  char pad;
  char d_type;
};


#define SYS_STRUCT_ADDITIONS
#undef SYS_STRUCT_ADDITIONS

void make_dd_dir(struct dir* cwd) {
}

void init_dir_system(void) {
  struct dir* dir_head = NULL;
  struct dir* dir_tail = NULL;
  dir_head = malloc(sizeof(struct dir));
  dir_head->dirfd = DEFAULT_DIRFD;
  dir_head->dir_name = "home";
  dir_head = NULL;
  dir_tail = dir_head;
}

void init_file_system(void) {
}

// literally copying the old file wouldnt do it.
// the question is how to do it given our current sys struct?
// we could add a flag or something plus a pointer of an fd that
// points to the actual file.
// link
int env____syscall9(int which, int* varargs) {
  const char* oldpath = (const char*)varargs[0];
  const char* newpath = (const char*)varargs[1];

  return 0;
}

// unlink
int env____syscall10(int which, int* varargs) {
  const char* pathname = (const char*)varargs[0];

  return 0;
}

// chdir
int env____syscall12(int which, int* varargs) {
  const char* pathname = (const char*)varargs[0];

  return 0;
}

// rename
int env____syscall38(int which, int* varargs) {
  const char* oldpath = (const char*)varargs[0];
  const char* newpath = (const char*)varargs[1];

  return 0;
}

// mkdir
int env____syscall39(int which, int* varargs) {
  const char* pathname = (const char*)varargs[0];
  mode_t mode = varargs[1];
  return 0;
}

// rmdir
int env____syscall40(int which, int* varargs) {
  const char* pathname = (const char*)varargs[0];

  return 0;
}

// symlink
int env____syscall83(int which, int* varargs) {
  const char* target = (const char*)varargs[0];
  const char* linkpath = (const char*)varargs[1];

  return 0;
}

// fchdir
int env____syscall133(int which, int* varargs) {
  int fd = varargs[0];

  return 0;
}

// getcwd
int env____syscall183(int which, int* varargs) {
  char* buf = (char*)varargs[0];
  size_t size = varargs[1];

  return 0;
}

// getdents
// we wont probably need this one
int env____syscall220(int which, int* varargs) {
  unsigned int fd = varargs[0];
  struct linux_dirent* drip = (struct linux_dirent*)varargs[1];
  unsigned int count = varargs[2];

  return 0;
}

// mkdirat
int env____syscall296(int which, int* varargs) {
  const char* name = (const char*)varargs[0];
  mode_t mode = varargs[1];

  return 0;
}

// unlinkat
int env____syscall301(int which, int* varargs) {
  int dirfd = varargs[0];
  const char* pathname = (const char*)varargs[1];
  int flags = varargs[2];

  return 0;
}

// renameat
int env____syscall302(int which, int* varargs) {
  int olddirfd = varargs[0];
  const char* oldpath = (const char*)varargs[1];
  int newdirfd = varargs[2];
  const char* newpath = (const char*)varargs[3];

  return 0;
}

// symlinkat
int env____syscall304(int which, int* varargs) {
  const char* target = (const char*)varargs[0];
  int newdirfd = varargs[1];
  const char* linkpath = (const char*)varargs[2];

  return 0;
}
#endif

