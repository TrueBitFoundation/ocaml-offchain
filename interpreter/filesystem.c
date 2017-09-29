
#include <stdlib.h>

char inputName(int, int);
int inputSize(int);
char inputData(int, int);

struct system {
  int next_fd;
  int ptr[1024]; // Pointers to the data blocks for each fd
  int pos[1024]; // Location inside the block
  char *file_name[1024];
  char *file_data[1024];
  int file_size[1024];
};

// Global variable that will store our system
struct system *sys;

int getNameLength(int ptr) {
  int res = 0;
  while (inputName(ptr, res) != 0) res++;
  return res;
}

char *getName(int ptr) {
  int sz = getNameLength(ptr);
  char *res = malloc(sz+1);
  for (int i = 0; i < sz; i++) res[i] = inputName(ptr, i);
  res[sz] = 0;
  return res;
}

char *getData(int ptr) {
  int sz = inputSize(ptr);
  char *res = malloc(sz+1);
  for (int i = 0; i < sz; i++) res[i] = inputData(ptr, i);
  return res;
}

void initSystem() {
  struct system *s = malloc(sizeof(struct system));
  s->next_fd = 3; // 0:stdin, 1:stdout, 2:stderr
  // Actually we should here have a list of file names?
  // Read input byte by byte, it includes file names and data
  int loc = 0;
  int index = 0;
  int nextLength = getNameLength(index);
  while (nextLength > 0) {
     s->file_name[index] = getName(index);
     s->file_size[index] = inputSize(index);
     s->file_data[index] = getData(index);
     index++;
     nextLength = getNameLength(index);
  }
  s->file_name[index] = 0;
  sys = s;
}

int str_eq(char *s1, char *s2) {
   while (*s1 == *s2) {
     if (!s1[0] && !s2[0]) return 1;
     s1++;
     s2++;
   }
   return 0;
}

int ___syscall5(int which, int *varargs) {
  char *name = varargs[0];
  int flags = varargs[1];
  int mode = varargs[2];
  // No empty names allowed
  if (!name || !name[0]) return -1;
  int index = 0;
  while (sys->file_name[index]) {
      if (str_eq(sys->file_name[index], name)) {
              int fd = sys->next_fd;
              sys->ptr[fd] = index;
              sys->pos[fd] = 0;
              sys->next_fd++;
              return fd;
      }
      index++;
  }
  // No such file
  return -1;
}

