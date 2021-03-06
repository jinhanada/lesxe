#include <stdio.h>

int main(int argc, char** argv) {
    if (argc != 2) {
      fprintf(stderr, "Usage: cat FILE | %s VARNAME > CFILE\n", argv[0]);
      return 1;
    }

    int c;
    int col;
    printf("char %s[] = {", argv[1]);
    while((c = fgetc(stdin)) != EOF) {
        if (col % 12 == 0) printf("\n    ");
        col++;
        printf("0x%02x, ", c);
    }
    printf("0"); // put terminating zero
    printf("\n};\n");

    return 0;
}
