
/***********************************************************************************************/
#ifndef SYS_CALL_H
#define SYS_CALL_H
/***********************************************************************************************/
#define LIMIT_DIR_DEPTH 32U
#define LIMIT_CHAR_NUMBER 100U
#define LIMIT_NUM_DIR
#define PTR_SZ sizeof(void*)
#define INT_SZ sizeof(int)

typedef int bool;
#define true 1
#define false 0

// dirfd of the default directory we would be in when the interpreter starts
#define DEFAULT_DIRFD 10000
#define DEFAULT_DIR_FD 1000
#define DEFAULT_FILE_FD 1000

#define ERR_ABS_PATH "absolute paths are not allowed. you should only use relative paths."
#define ERR_DBL_PARENTS "file or directory has two parents."
/***********************************************************************************************/
// denotes whether we are moving up or down the directory tree
enum direction_t {up = 0, down = 1, none = 2};

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
/***********************************************************************************************/
/*FIXME-use map instead of linked-list*/
struct file {
  int fd;
  char* file_name;
  char* data;
  int file_size;
  struct dir* parent_dir;

  struct file* next;
};

struct dir {
  int dirfd; // directory fd
  char* dir_name; // the directory name
  struct file* fds; // the fds under this dir
  int* dir_fds; // the dirfds under this dir
  // in simple terms, this is ..
  struct dir* parent_dir; // pointer to the parent dir
  // pionter to the next dir struct
  struct dir* next;
};

// just a measure to mitigate the fds growing without bounds
struct soft_cache {
  int next_free_fd;
  int next_free_dirfd;
  struct dir* cwd;
};
/***********************************************************************************************/
// directory head & tail
extern struct dir* dir_head;
extern struct dir* dir_tail;
extern struct soft_cache fs_cache;

// file head & tail
extern struct file* file_head;
extern struct file* file_tail;

extern const int sizeof_ptr;
extern const int sizeof_int;

// Global variable that will store our system
extern struct system *sys;
/***********************************************************************************************/
#endif

