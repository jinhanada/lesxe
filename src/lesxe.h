#ifndef __LESXE_H
#define __LESXE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <assert.h>


// Types & Structs
// =============================================================================

typedef struct LeObj LeObj;
typedef struct LeVM  LeVM;

enum { Le_OK, Le_ERR, Le_Break, Le_Continue, Le_EOF };

enum {
      Le_nil,   Le_int,
      Le_array, Le_symbol, Le_pair, Le_func, Le_user,
      Le_bytes, Le_string,
      Le_unknown
};


// LeObj & LeVM
// =============================================================================

LeObj*   le_int2obj(intptr_t n);
intptr_t le_obj2int(LeObj* p);

int le_typeof(LeObj* p);
int le_is_nil(LeObj* p);
int le_is_num(LeObj* p);
int le_is_obj(LeObj* p);
int le_is_array(LeObj* p);
int le_is_symbol(LeObj* p);
int le_is_pair(LeObj* p);
int le_is_func(LeObj* p);
int le_is_bytes(LeObj* p);
int le_is_string(LeObj* p);

LeObj* le_get_hash(LeObj* x);

int le_vm_cells(LeVM* vm);
LeObj* le_vm_result(LeVM* vm);
LeObj* le_vm_error(LeVM* vm);


// Memory, GC, and Temporary Stack
// =============================================================================
void   le_gc(LeVM* vm);
int    le_push(LeVM* vm, LeObj* obj);
LeObj* le_pop(LeVM* vm);
LeObj* le_stack_at(LeVM* vm, int i);
int    le_stack_index(LeVM* vm);
void   le_set_stack(LeVM* vm, int i, LeObj* x);
void   le_restore_stack(LeVM* vm, int index);


// Data Types
// =============================================================================
LeObj* le_new_array(LeVM* vm, int len);
int    le_array_len(LeObj* xs);

LeObj* le_cons(LeVM* vm, LeObj* a, LeObj* b);
LeObj* le_car(LeObj* xs);
LeObj* le_cdr(LeObj* xs);
void   le_set_car(LeObj* xs, LeObj* x);
void   le_set_cdr(LeObj* xs, LeObj* x);
LeObj* le_second(LeObj* xs);
LeObj* le_third(LeObj* xs);
LeObj* le_reverse_inplace(LeObj* xs);

LeObj* le_new_str(LeVM* vm, int len);
LeObj* le_new_str_from(LeVM* vm, char* str);
char*  le_cstr_of(LeObj* str);
int    le_str_len(LeObj* s);
char   le_str_get(LeObj* s, int i);
LeObj* le_str_concat(LeVM* vm, LeObj* a, LeObj* b);
int    le_str_eq(LeObj* a, LeObj* b);

LeObj* le_new_bytes(LeVM* vm, int len);
int    le_bytes_len(LeObj* xs);

LeObj* le_new_sym_from(LeVM* vm, char* name);
LeObj* le_new_sym(LeVM* vm, LeObj* name);

LeObj* le_new_func(LeVM* vm, LeObj* code, LeObj* env, LeObj* params);


// VM
// =============================================================================
LeVM* le_new_vm(int cells, int repl_buf_size);
LeVM* le_create_vm();
void  le_free_vm(LeVM* vm);


// Errors
// =============================================================================
int le_raise(LeVM* vm, LeObj* err);
int le_raise_with(LeVM* vm, LeObj* error, LeObj* x);


// S Expression Reader
// =============================================================================
int   le_read_str(LeVM* p, char* src);
char* le_to_str(LeObj* x);
char* le_err_str(LeVM* vm);


// Eval
// =============================================================================
int le_eval(LeVM* vm, LeObj* expr);
int le_eval_str(LeVM* vm, char* str);
int le_load_file(LeVM* vm, char* fname);
int le_load_corelib(LeVM* vm);


// REPL
// =============================================================================
int le_repl(LeVM* vm);


#endif
