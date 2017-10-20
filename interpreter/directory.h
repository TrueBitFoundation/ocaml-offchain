
#include "syscall.h"

bool is_relative(const char* pathname);
int num_relative(const char* pathname);
int getfd_fromname_file(const char* name);
const char* getname_fromfd_file(int fd);
int getfd_fromname_dir(const char* name);
const char* getname_fromfd_dir(int fd);
struct dir* newdir_fd(int fd);
struct file* newfile_fd(int fd);
char* dissect_path(const char* pathname);
char* read_dissected_path(const char* dissected_path, int n);
void illuminate_path(struct dir* cwd, const char* dissected_path);
void make_dd_dir(struct dir* cwd);
void init_dir_system(void);
void init_file_system(void);
void init_system(void);
void shut_down(void);
int env____syscall9(int which, int* varargs);
int env____syscall10(int which, int* varargs);
int env____syscall12(int which, int* varargs);
int env____syscall38(int which, int* varargs);
int env____syscall39(int which, int* varargs);
int env____syscall40(int which, int* varargs);
int env____syscall83(int which, int* varargs);
int env____syscall133(int which, int* varargs);
int env____syscall183(int which, int* varargs);
int env____syscall220(int which, int* varargs);
int env____syscall296(int which, int* varargs);
int env____syscall301(int which, int* varargs);
int env____syscall302(int which, int* varargs);
int env____syscall304(int which, int* varargs);
