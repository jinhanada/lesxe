#include "lesxe.h"

static void debugPrint(char* filename, int line, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  fprintf(stderr, "\e[31m");
  fprintf(stderr, "%s:%d ", filename, line);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\e[0m\n");
}


#define DBG(...)  { debugPrint(__FILE__, __LINE__, __VA_ARGS__); }
#define DIE(...) { DBG(__VA_ARGS__) exit(1); }

int main(int argc, char** argv) {
  LeVM* vm = le_create_vm();

  int code = le_load_file(vm, "./src/core.le"); // TODO: amalgamate!
  if (code != Le_OK) DIE("%s", le_err_str(vm));

  printf("lesxe stage: sprout\n");
  int cells = le_vm_cells(vm);
  printf("vm cells: %d (%ld MiB)\n", cells, cells * sizeof(void*) / 1024 / 1024);
  printf("Ctrl-D to exit!\n");
  
  le_repl(vm);
  
  le_free_vm(vm);

  printf("bye!\n");
 
  return 0;
}
