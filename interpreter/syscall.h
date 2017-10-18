
/**************************************************************************************************/
#ifndef SYS_CALL_H
#define SYS_CALL_H
/**************************************************************************************************/
#define LIMIT_NUM_DIR
/**************************************************************************************************/
struct system {
  int next_fd;
  int ptr[1024]; // Pointers to the data blocks for each fd
  int pos[1024]; // Location inside the block
  int closed[1024];

  struct piece *file_output[1024];
  unsigned char *file_name[1024];
  unsigned char *file_data[1024];
  int file_size[1024];
  
  int call_record; // file descriptor for call record
};

// Global variable that will store our system
extern struct system *sys;
/**************************************************************************************************/
/*FIXME-use map instead of linked-list*/
struct file {
  int fd;
  char* file_name;
  char* data;
  int file_size;
  struct dir* parent_dir;


  struct file* next;
};

// file head & tail
extern struct file* file_head;
extern struct file* file_tail;

// dirfd of the default directory we would be in when the interpreter starts
#define DEFAULT_DIRFD 10000

struct dir {
  int dirfd; // directory fd
  char* dir_name; // the directory name
  int* fds; // the fds under this dir
  int* dir_fds; // the dirfds under this dir
  // in simple terms, this is a pointer to ..
  struct dir* prev_dirfd; // pointer to the parent dir
  // pionter to the next dir struct
  struct dir* next;
};

// directory head & tail
extern struct dir* dir_head;
extern struct dir* dir_tail;

// just a measure to mitigate the fds growing without bounds
struct soft_cache {
  int next_free_fd;
  int next_free_dirfd;
  struct dir* cwd;
};

extern struct soft_cache fs_cache; 
/**************************************************************************************************/
#endif

