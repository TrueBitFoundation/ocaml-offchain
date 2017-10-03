
#include <stdlib.h>

int inputSize(int);
unsigned char inputName(int, int);
unsigned char inputData(int, int);

void outputSize(int, int);
void outputName(int, int, unsigned char);
void outputData(int, int, unsigned char);

int debugString(char *dta);
int debugSeek(int c);
void debugRead(int c);
int debugReadCount(int c);

// Output to a linked list, not a linear block?
struct piece {
  unsigned char *data;
  int size;
  struct piece *prev;
};

struct system {
  int next_fd;
  int ptr[1024]; // Pointers to the data blocks for each fd
  int pos[1024]; // Location inside the block
  int closed[1024];

  struct piece *file_output[1024];
  unsigned char *file_name[1024];
  unsigned char *file_data[1024];
  int file_size[1024];
};

// Global variable that will store our system
struct system *sys;

int getNameLength(int ptr) {
  int res = 0;
  while (inputName(ptr, res) != 0) res++;
  return res;
}

unsigned char *getName(int ptr) {
  int sz = getNameLength(ptr);
  unsigned char *res = malloc(sz+1);
  for (int i = 0; i < sz; i++) res[i] = inputName(ptr, i);
  res[sz] = 0;
  return res;
}

unsigned char *getData(int ptr) {
  int sz = inputSize(ptr);
  unsigned char *res = malloc(sz);
  for (int i = 0; i < sz; i++) res[i] = inputData(ptr, i);
  return res;
}

unsigned char* copyBytes(unsigned char* bytes, int len) {
  unsigned char* res = malloc(len);
  for (int i = 0; i < len; i++) res[i] = bytes[i];
  return res;
}

void addPiece(int idx, unsigned char *bytes, int len) {
  struct piece *p = malloc(sizeof(struct piece));
  p->prev = sys->file_output[idx];
  p->data = copyBytes(bytes, len);
  p->size = len;
  sys->file_output[idx] = p;
}

void initSystem() {
  struct system *s = malloc(sizeof(struct system));
  s->ptr[0] = -1;
  s->ptr[1] = -2;
  s->ptr[2] = -3;
  s->next_fd = 3; // 0:stdin, 1:stdout, 2:stderr
  // Actually we should here have a list of file names?
  // Read input byte by byte, it includes file names and data
  int loc = 0;
  int index = 0;
  int nextLength = getNameLength(index);
  while (nextLength > 0) {
     s->file_output[index] = 0;
     s->file_name[index] = getName(index);
     s->file_size[index] = inputSize(index);
     s->file_data[index] = getData(index);
     index++;
     nextLength = getNameLength(index);
  }
  s->file_name[index] = 0;
  sys = s;
}

void finalizeSystem() {
  int index = 0;
  while (sys->file_name[index]) {
    // output name
    unsigned char *name = sys->file_name[index];
    int i = 0;
    while (*name) {
      outputName(index, i, *name);
      name++;
      i++;
    }
    // If there is no output, then output the linear block in case it was changed
    if (!sys->file_output[index]) {
      outputSize(index, sys->file_size[index]);
      unsigned char *data = sys->file_data[index];
      int i = 0;
      while (*data) {
        outputData(index, i, *data);
        data++;
        i++;
      }
    }
    else {
       // Calculate size
       int sz = 0;
       struct piece *p = sys->file_output[index];
       while (p->prev) {
         sz += p->size;
         p = p->prev;
       }
       outputSize(index, sz);
       p = sys->file_output[index];
       while (p->prev) {
         sz -= p->size;
         for (int i = 0; i < p->size; i++) {
           outputData(index, sz+i, p->data[i]);
         }
         p = p->prev;
       }
    }
    index++;
  }
}

int str_eq(unsigned char *s1, unsigned char *s2) {
   while (*s1 == *s2) {
     if (!s1[0] && !s2[0]) return 1;
     s1++;
     s2++;
   }
   return 0;
}

// Open file
int env____syscall5(int which, int *varargs) {
  unsigned char *name = (unsigned char*)varargs[0];
  int flags = varargs[1];
  int mode = varargs[2];
  // No empty names allowed
  if (!name || !name[0]) return -1;
  debugString((char*)name);
  int index = 0;
  if (!sys) return -1;
  while (sys->file_name[index]) {
      if (str_eq(sys->file_name[index], name)) {
              int fd = sys->next_fd;
              sys->ptr[fd] = index;
              sys->pos[fd] = 0;
              sys->closed[fd] = 0;
              sys->next_fd++;
              return fd;
      }
      index++;
  }
  // No such file
  return -1;
}

// Seeking
int env____syscall140(int which, int *varargs) {
  int fd = varargs[0];
  int offset_high = varargs[1];
  int offset_low = varargs[2];
  int *result = (int*)varargs[3];
  int whence = varargs[4];
  // llseek(stream, offset_low, whence)
  if (whence == 1) {
    sys->pos[fd] += offset_low;
  }
  // Maybe this is seeking from end?
  else if (whence == 2) {
    int sz = sys->file_size[sys->ptr[fd]];
    sys->pos[fd] = sz + offset_low;
    debugSeek(offset_low);
  }
  else return -1;
  *result = sys->pos[fd];
  if (sys->pos[fd] < 0) return -1;
  return 0;
}

// Close
int env____syscall6(int which, int *varargs) {
  int fd = varargs[0];
  sys->closed[fd] = 1;
  return 0;
}

// Read
int env____syscall3(int which, int *varargs) {
  int fd = varargs[0];
  unsigned char *buf = (unsigned char*)varargs[1];
  int count = varargs[2];
  debugReadCount(count);
  // read
  int index = sys->ptr[fd];
  int pos = sys->pos[fd];
  int i;
  for (i = 0; i < count && i+pos < sys->file_size[index]; i++) {
    buf[i] = sys->file_data[index][pos+i];
    debugRead(buf[i]);
  }
  sys->pos[fd] += i;
  return i;
}

struct stat {
};

// Stat
int env____syscall195(int which, int *varargs) {
  unsigned char *path = (unsigned char*)varargs[0];
  struct stat *stats = (struct stat*)varargs[1];
  // Invent some stats
  debugString((char*)path);
  return -1;
}

// Write
int env____syscall4(int which, int *varargs) {
  int fd = varargs[0];
  unsigned char *buf = (unsigned char*)varargs[1];
  int count = varargs[2];
  debugString((char*)buf);
  if (sys->ptr[fd] < 0) return count;
  addPiece(sys->ptr[fd], buf, count);
  return count;
}

