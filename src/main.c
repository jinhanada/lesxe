#include "lesxe.h"
#include <getopt.h>

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

typedef struct {
  int development;
  int quiet;
} Options;

char* USAGE =
  "Usage: lesxe [options] [script]\n"
  "Options:\n"
  "    -l source  load source before run script/repl\n"
  "    -D         development mode, no library loaded\n"
  "    -q         quiet\n"
  "    -h         show this help\n"
  "Example:\n"
  "    lesxe -l a.le -l b.le main.le\n"
  "    lesxe -l a.le\n"
  ;

void usage() {
  printf("%s", USAGE);
}

void load_file(LeVM* vm, char* fname) {
  int code = le_load_file(vm, fname);
  if (code != Le_OK) DIE("%s", le_err_str(vm));  
}

void load_corelib(LeVM* vm) {
  int code = le_load_corelib(vm);
  if (code != Le_OK) DIE("%s", le_err_str(vm));
}

void run_repl(Options* opts, LeVM* vm) {
  if (!opts->quiet) {
    printf("lesxe stage: sprout\n");
    if (opts->development) {
      printf("!!DEVELOPMENT MODE!!\n");
    }
    int cells = le_vm_cells(vm);
    printf("vm cells: %d (%ld MiB)\n", cells, cells * sizeof(void*) / 1024 / 1024);
    printf("Ctrl-D to exit!\n");
  }

  le_repl(vm);

  if (!opts->quiet) {
    printf("bye!\n");
  }
}

int handle_opts(Options* opts, LeVM* vm, int argc, char* argv[]) {
  // returns start index of rest arguments(optind)
  const char* optstr = "hql:D";
  opterr = 0; // disable logging error
  int c;

  while ((c = getopt(argc, argv, optstr)) != -1) {
    switch (c) {
    case 'h':
      usage();
      exit(0);
    case 'q':
      opts->quiet = 1;
      break;
    case 'l':
      load_file(vm, optarg);
      break;
    case 'D':
      opts->development = 1;
      break;
    case '?':
      switch (optopt) {
      case 'l':
        fprintf(stderr, "No filename given for option -l\n");
      default:
        fprintf(stderr, "Unknown option: %c\n", optopt);
      }
      usage();
      exit(0);
    }
  }

  return optind;
}

int main(int argc, char** argv) {
  LeVM* vm = le_create_vm();

  Options opts = { .development = 0, .quiet = 0 };

  int argi      = handle_opts(&opts, vm, argc, argv);
  int rest_argc = argc - argi;

  if (!opts.development) load_corelib(vm);

  if (rest_argc > 0) {
    load_file(vm, argv[argi]);
  } else {
    run_repl(&opts, vm);
  }
  
  le_free_vm(vm);
  return 0;
}
