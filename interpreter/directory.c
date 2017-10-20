
/***********************************************************************************************/
#ifndef DIRECTORY_C
#define DIRECTORY_C
/***********************************************************************************************/
#include "syscall.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
/**********************************************************************************************/
#define DEBUG_DUMP 1

#define mode_t int
#define MAKE_NEW_DIR_NODE(_x1) struct dir* (_x1) = malloc(sizeof(struct dir))
#define MAKE_NEW_FILE_NODE(_x1) struct file* (_x1) = malloc(sizeof(struct file))
#define uintptr_t varag_ptr;
#define IS_RELATIVE(_x1) (('.' == (_x1[0])) ? true : false)
#define DIR_EXISTS(_x1)
#define FILE_EXISTS(_x1)
/**********************************************************************************************/
struct linux_dirent {
  unsigned long d_ino;
  unsigned long d_off;
  unsigned short d_reclen;
  char* dname;
  char pad;
  char d_type;
};

struct dir* dir_head = NULL;
struct dir* dir_tail = NULL;
struct file* file_head = NULL;
struct file* file_tail = NULL;
struct soft_cache fs_cache = {DEFAULT_FILE_FD, DEFAULT_DIR_FD, NULL};

const int sizeof_int = INT_SZ;
const int sizeof_ptr = PTR_SZ;
/***********************************************************************************************/
bool is_relative(const char* pathname) {
  if (pathname[0] == '.') return true;
  else return false;
}

int num_relative(const char* pathname) {
  int count = 0;
  for (int i = 0; pathname[i] != '\0'; ++i) {
    if ('/' == pathname[i]) count++;
  }
  return count + 1;
}

int getfd_fromname_file(const char* name) {
  struct file* current = file_head;
  int ret = -1;
  while(current != NULL) {
    if (strcmp(name, current->file_name)) {
      ret = current->fd;
      break;
    }
    current = current->next;
  }
  return ret;
}

// will return the first name matching
const char* getname_fromfd_file(int fd) {
  struct file* current = file_head;
  char* ret;
  while(current != NULL) {
    if (fd == current->fd) {
      ret = current->file_name;
      break;
    }
    current = current->next;
  }
  return ret;
}

int getfd_fromname_dir(const char* name) {
  struct dir* current = dir_head;
  int ret = -1;
  while(current != NULL) {
    if (strcmp(name, current->dir_name)) {
      ret = current->dirfd;
      break;
    }
    current = current->next;
  }
  return ret;
}

// will return the first name matching
const char* getname_fromfd_dir(int fd) {
  struct dir* current = dir_head;
  char* ret;
  while(current != NULL) {
    if (fd == current->dirfd) {
      ret = current->dir_name;
      break;
    }
    current = current->next;
  }
  return ret;
}

struct dir* newdir_fd(int fd) {
  struct dir* dummy = malloc(sizeof(struct dir));
  dummy->dirfd = fd;
  dir_tail->next = dummy;
  dir_tail = dummy;
  return dummy;
}

struct file* newfile_fd(int fd) {
  struct file* dummy = malloc(sizeof(struct file));
  dummy->fd = fd;
  file_tail->next = dummy;
  file_tail = dummy;
  return dummy;
}

char* dissect_path(const char* pathname) {
  char* dissected_path = calloc(LIMIT_DIR_DEPTH * LIMIT_CHAR_NUMBER, sizeof(char));
  int count = 0;
  int count2 = 0;
  int j = 0;
  printf("%s\n", pathname);
  while(*(pathname + j) != '\0') {
    if ('/' == *(pathname + j)) {
      dissected_path[count* LIMIT_CHAR_NUMBER + count2 + 1] = '\0';
      count++;
      count2 = 0;
      j++;
      continue;
    }
    dissected_path[count* LIMIT_CHAR_NUMBER + count2] = *(pathname + j);
    count2++;
    j++;
  }
#if DEBUG_DUMP
  for (int i = 0; i < 32; ++i) {
    if (dissected_path[i*LIMIT_CHAR_NUMBER] !='\0') {
      for (j = 0; j < LIMIT_CHAR_NUMBER; ++j) {
        printf("%c", dissected_path[i*LIMIT_CHAR_NUMBER + j]);
      }
      printf("\n");
    }
  }
#endif
  return dissected_path;
}

char* read_dissected_path(const char* dissected_path, int n) {
  char* ret = calloc(LIMIT_CHAR_NUMBER, sizeof(char));
  //if (n >= LIMIT_DIR_DEPTH) ret[0] = '\0';
  if (dissected_path[n*LIMIT_CHAR_NUMBER] !='\0') {
    for (int j = 0; j < LIMIT_CHAR_NUMBER; ++j) {
      ret[j] = dissected_path[n*LIMIT_CHAR_NUMBER + j];
    }
  }
  return ret;
}

void illuminate_path(struct dir* cwd, const char* dissected_path) {
  struct dir* walking = cwd;
  struct dir* current = dir_head;
  char* path = read_dissected_path(dissected_path, 0);
  enum direction_t direction;
  struct dir* new;
  if (path[0] == '.') {
    if (path[1] == '.') direction = down;
    else {
      direction = up;
    }
  }
  bool match = false;
  for (int i = 1; i < LIMIT_DIR_DEPTH; ++i) {
    path = read_dissected_path(dissected_path, i);
    if (path[0] != '\0') {
      if (direction == up) {
        if (walking->parent_dir) {
          walking = walking->parent_dir;
        } else {
          new = newdir_fd(fs_cache.next_free_dirfd);
          walking->parent_dir = new;
          walking = walking->parent_dir;
          fs_cache.next_free_dirfd++;
        }
      } else {
        while (current->next != NULL) {
          if (current->dir_name == path && current->parent_dir->dirfd == walking->dirfd) {
              walking = current;
              match = true;
            }
          current = current->next;
        }
        if (!match) {
          new = newdir_fd(fs_cache.next_free_dirfd);
          new->parent_dir = walking;
          walking = new;
          fs_cache.next_free_dirfd++;
        }
      }
    }
  }
}

void make_dd_dir(struct dir* cwd) {
  MAKE_NEW_DIR_NODE(dd);
}

void init_dir_system(void) {
  dir_head = malloc(sizeof(struct dir));
  dir_tail = malloc(sizeof(struct dir));
  struct dir* dir_tail = NULL;
  dir_head->dirfd = DEFAULT_DIRFD;
  // should be mapped to the pwd of the task
  dir_head->dir_name = "placeholder";
  dir_head->next = NULL;
  dir_tail = dir_head;
  fs_cache.cwd = dir_head;
}

void init_file_system(void) {
  file_head = malloc(sizeof(struct file));
  file_tail = file_head;
}

void init_system(void) {
  init_dir_system();
  init_file_system();

  int sizeof_ptr = PTR_SZ;
  int sizeof_int = INT_SZ;
}

void shut_down(void) {
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
  if (!IS_RELATIVE(pathname)) return -1;
  enum direction_t direction;
  if (pathname[0] == '.') {
    if (pathname[1] == '.') direction = up;
    else direction = down;
  }
  char* dissected_path = dissect_path(pathname);
  illuminate_path(fs_cache.cwd, dissected_path);
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
/***********************************************************************************************/
#endif
/***********************************************************************************************/

