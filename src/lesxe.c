#include "lesxe.h"
#include "corelib.h"


// Conventions
// =============================================================================
/*
  Public le_public_function
  static privateFunction
  MacroProc()
  MACRO_VALUE
  local_variable
*/


// Default Values
// =============================================================================

#define VM_DEFAULT_CELLS (10 * 1024 * 1024 / sizeof(void*)) /* 10Mib */
#define VM_DEFAULT_REPL_BUF_SIZE 4096
#define TEMPORARY_STACK_SIZE 2048


// Types and Structs
// =============================================================================

// ===== Symbol =====

enum {
      SymNil = 0,
      SymTrue,
      SymQuote, SymQuasiquote,
      SymUnquote, SymUnquoteSplicing,
      SymLet, SymFn, SymDef, SymIf, SymSet,
      SymWhile, SymBreak, SymContinue,
      SymApply, SymCatch, SymPreEval,

      /* types */
      SymNumber, SymArray, SymSymbol, SymPair, SymFunc, SymBytes, SymString,
      
      /* primitives */
      /* arithmetics */ SymPrimAdd, SymPrimSub, SymPrimMul, SymPrimDiv, SymPrimMod,
      /* compare */ SymPrimEq, SymPrimNot, SymPrimGt,
      /* types */ SymPrimTypeOf, SymPrimHash,
      /* array */ SymPrimArrayNew, SymPrimArrayGet, SymPrimArraySet, SymPrimArrayLen,
      /* pair */ SymPrimCons, SymPrimCar, SymPrimCdr, SymPrimSetCar, SymPrimSetCdr,
      /* symbol */ SymPrimSymNew, SymPrimSymStr,
      /* string */
      SymPrimStr, SymPrimStrLen, SymPrimStrGet, SymPrimStrEq, SymPrimStrCat,
      SymPrimStrMake,
      /* bytes */
      SymPrimBytesNew, SymPrimBytesGet, SymPrimBytesSet, SymPrimBytesLen,
      /* i/o */
      SymPrimPutc, SymPrimGetc, SymPrimPrint,
      SymPrimReadTextFile, SymPrimWriteTextFile,
      /* read */ SymPrimReadStr,
      /* error */ SymPrimRaise,
      /* system */ SymPrimExit, SymPrimGC, SymPrimLoadFile,
      
      /* errors */
      SymError, SymUndefinedSymbol,
      SymMalformedFn, SymInvalidArgs,
      SymExpectInteger, SymZeroDivision,
      SymFileNotFound, SymInvalidPreEvalProc,
      SymInvalidFileDescriptor, SymNotAProc,
      SymOutOfRange, SymUnknownError,

      SymTableSize
};


// ===== Object =====
// - StringはBytesをそのまま使用

typedef struct {
  LeObj* data[0];
} Array;

typedef struct {
  LeObj* car;
  LeObj* cdr;
} Pair;

typedef struct {
  LeObj* name;
  LeObj* global;
  LeObj* prim;
} Symbol;

typedef struct {
  LeObj* code;
  LeObj* env;
  LeObj* vars;
} Func;

typedef struct {
  intptr_t size;
  uint8_t  data[];  
} Bytes;

typedef struct {
  LeObj* symbols;
  LeObj* reserved;
} Root;

struct LeObj {
  intptr_t header;
  LeObj*   hash;
  union {
    Array   Array;
    Symbol  Symbol;
    Pair    Pair;
    Func Func;
    Bytes   Bytes;
    Root    Root;
  };
};

#define OBJECT_HEADER_CELLS 2 // header and hash


// ===== VM =====

struct LeVM {
  // parser
  char* src;
  char* p;
  // Memory
  intptr_t cells;
  LeObj**  new;
  LeObj**  old;
  LeObj**  here;
  LeObj**  scanned;
  LeObj**  tmp;     // save temporary object from GC
  int      tp;      // temporary stack pointer
  // Interpreter
  LeObj* root;
  LeObj* env;
  LeObj* result;
  LeObj* err;
  // Symbols for O(1) access
  LeObj* symTable[SymTableSize];
  // REPL
  int replBufSize;
};

  
// Shorthands
// =============================================================================
// いくつかのマクロはLeVM* vmやint codeの定義を前提としている

typedef intptr_t Cell;
typedef uint8_t  Byte;
#define Public
#define Push(x)                le_push((vm), (x))
#define Pop()                  le_pop(vm)
#define Get(i)                 le_stack_at(vm, i)
#define Set(i, x)              le_set_stack(vm, i, x)
#define SaveStack              int _tp = le_stack_index(vm)
#define RestoreStack           le_restore_stack(vm, _tp)
#define RestoreReturn(code)    { RestoreStack; return code; }
#define Cons(vm, a, b)         le_cons((vm), (a), (b))
#define Car(x)                 le_car(x)
#define Cdr(x)                 le_cdr(x)
#define SetCar(xs, x)          le_set_car(xs, x)
#define SetCdr(xs, x)          le_set_cdr(xs, x)
#define Second(x)              le_second(x)
#define Third(x)               le_third(x)
#define Obj                    LeObj*
#define Sym(name)              (vm->symTable[Sym##name])
#define OBJ_SIZE(structure)    (sizeof(structure)/sizeof(Cell))
#define ExpectOK               if (code != Le_OK) return code;
#define RaiseWith(e, xs)       (le_raise_with(vm, Sym(e), xs))
#define ExpectType(t, v)       if (!le_is_##t(v)) return RaiseWith(InvalidArgs, v)


// Forward declarations
// =============================================================================
static char* toStr(Obj);
static void setupVM(LeVM* vm);

// Debug print
// =============================================================================

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


// File utilities
// =============================================================================

static char* readTextFile(char* fname) {
  FILE* file = fopen(fname, "r");
  if (!file) return NULL;

  fseek(file, 0, SEEK_END);
  int flen   = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* text = calloc(sizeof(char), flen+1);

  if (fread(text, sizeof(char), flen, file) < flen) {
    fclose(file);
    return NULL;
  }

  text[flen] = '\0';
  fclose(file);

  return text;
}

static int writeTextFile(char* fname, char* s) {
  FILE* file = fopen(fname, "w");
  if (!file) return 0;

  fprintf(file, "%s", s);
  fclose(file);
  return 1;
}


// LeObj & LeVM
// =============================================================================

/*
  Cheney's CopyGC

  タグ付きポインタ
  -----
  最下位ビットが1なら63bit整数(num)、0ならポインタ(obj)。特にポインタ0はnilを表す。
  nilはobjとしては扱わない。le_is_obj(nil) == 0。
*/

#define nil 0

/*
  ヘッダレイアウト
  -----
  00...000  0000 0
  |         |    |- 0:moved(pointer)
  |         |- data type(4bit)
  |- cells(59bit)

  ヘッダの最下位ビットが0ならmoved。逆じゃないよ！
  単にforward先のオブジェクトアドレスを入れるだけで良い。
*/

#define HEADER_CELLS_BIT  5 // << 5
#define HEADER_CELLS_MASK (-1   << HEADER_CELLS_BIT)
#define HEADER_TYPE_BIT   1 // << 1
#define HEADER_TYPE_MASK  (0x0F << HEADER_TYPE_BIT) // 1111

/*
  Data Type Tag
  データ型の最下位1ビットはbyte arrayベースかどうかを表す。
*/

#define T_ARRAY   0x00 // 0000
#define T_SYMBOL  0x02 // 0010
#define T_PAIR    0x04 // 0100
#define T_FUNC    0x06 // 0110
#define T_USER    0x0E // 1110 user defined
#define T_BYTES   0x01 // 0001
#define T_STRING  0x03 // 0011


#define ALIGN_CELL (sizeof(Cell)-1)
static Cell align(Cell n) {
  return (n + ALIGN_CELL) & ~ALIGN_CELL;
}


// ===== Tagged pointer =====

Public Obj le_int2obj(Cell n)   {
  return (Obj)(n << 1 ^ 0x01);
}

Public Cell le_obj2int(Obj p) {
  return (Cell)p >> 1;
}

Public int le_is_nil(Obj p) {
  return p == 0;
}

Public int le_is_num(Obj p) {
  return (Cell)p & 0x01;
}

Public int le_is_obj(Obj p) {
  return !le_is_nil(p) && !le_is_num(p);
}


// ===== Object header =====

static Cell setType(Cell header, int type) {
  assert(0 <= type && type < 16); // 4bit
  return (header & ~HEADER_TYPE_MASK) | (type << HEADER_TYPE_BIT);
}

static int typeOf(Cell header) {
  return (header & HEADER_TYPE_MASK) >> HEADER_TYPE_BIT;
}

static int isBytes(Cell header) {
  return typeOf(header) & 0x01;
}

static Cell setCells(Cell header, Cell cells) {
  assert(cells > 0);
  return (header & ~HEADER_CELLS_MASK) | (cells << HEADER_CELLS_BIT);
}

static Cell cellsOf(Cell header) {
  // 符号ビットが立ってしまっている可能性があるので、
  // 論理シフトをするために一度符号なしにキャスト
  return (Cell)(((uintptr_t)header) >> HEADER_CELLS_BIT);
}

static int isMoved(Cell header) {
  return !(header & 0x01);
}

static Cell makeHeader(Cell cells, int type) {
  Cell header = 0x01; // 最下位1bitが0ならmoved
  header = setType(header, type);
  header = setCells(header, cells);
  return header;
}


// ===== type checkers =====

Public int le_typeof(Obj p) {
  if (p == nil)           return Le_nil;
  if (le_is_num(p)) return Le_int;
  int t = typeOf(p->header);
  switch (t) {
  case T_ARRAY:   return Le_array;
  case T_SYMBOL:  return Le_symbol;
  case T_PAIR:    return Le_pair;
  case T_FUNC:    return Le_func;
  case T_USER:    return Le_user;
  case T_BYTES:   return Le_bytes;
  case T_STRING:  return Le_string;
  }
  return Le_unknown;
}

Public int le_is_array(Obj p) {
  return le_typeof(p) == Le_array;
}

Public int le_is_symbol(Obj p) {
  return le_typeof(p) == Le_symbol;
} 

Public int le_is_pair(Obj p) {
  return le_typeof(p) == Le_pair;
}

Public int le_is_func(Obj p) {
  return le_typeof(p) == Le_func;
}

Public int le_is_bytes(Obj p) {
  return le_typeof(p) == Le_bytes;
}

Public int le_is_string(Obj p) {
  return le_typeof(p) == Le_string;
}


// ===== Hash =====
// nil: 0
// number: そのもの
// string: 文字列の実体から計算
// other: 現在のアドレスを、2進数で最下位の固定ゼロが消えるようにシフトしたもの

static Cell hashDJB2(char* s) {
  Cell hash = 5381;
  for (char c = *s; c != '\0'; s++, c = *s) {
    hash = ((hash << 5) + hash) + c; // hash * 33 + c
  }
  
  return hash;
}

Public Obj le_get_hash(Obj obj) {
  // hash could be a negative integer
  if (obj == nil) return le_int2obj(0);
  if (le_is_num(obj)) return obj;

  // already have hash
  if (obj->hash != nil) return obj->hash;

  // string
  if (le_is_string(obj)) {
    Cell h = hashDJB2(le_cstr_of(obj));
    Obj hash = le_int2obj(h);
    obj->hash = hash; 
    return hash;
  }

  // normal object
  Cell h = ((Cell)obj) >> 3;
  Obj hash = le_int2obj(h);
  obj->hash = hash;
  return hash;
}


// ===== Temporary stack =====

Public int le_push(LeVM* vm, Obj obj) {
  if (vm->tp >= TEMPORARY_STACK_SIZE)
    DIE("temporary stack overflow");
  int i = vm->tp;
  vm->tmp[i] = obj;
  vm->tp++;
  return i;
}

Public Obj le_pop(LeVM* vm) {
  if (vm->tp <= 0)
    DIE("temporary stack underflow");
  vm->tp--;
  return vm->tmp[vm->tp];
}

Public Obj le_stack_at(LeVM* vm, int i) {
  return vm->tmp[i];
}

Public int le_stack_index(LeVM* vm) {
  return vm->tp;
}

Public void le_set_stack(LeVM* vm, int i, Obj x) {
  vm->tmp[i] = x;
}

Public void le_restore_stack(LeVM* vm, int index) {
  vm->tp = index;
}


// ===== For debug GC =====

static int objInNew(LeVM* vm, Obj obj) {
  return vm->new[0] <= obj && obj < vm->new[vm->cells];
}

static void assertAlive(LeVM* vm, Obj x) {
  if (!le_is_obj(x) || objInNew(vm, x)) return;
  DIE("old obj %p", x);
}

static void assertAllAlive(LeVM* vm) {
  vm->scanned = vm->new;
  while (vm->scanned < vm->here) {
    Obj  obj    = (Obj)vm->scanned;
    Cell cells  = cellsOf(obj->header);
    Cell actual = cells + OBJECT_HEADER_CELLS;
    vm->scanned += actual;

    // no need to check
    if (isBytes(obj->header)) continue;

    // check
    for (int i = 0; i < cells; i++) {
      assertAlive(vm, obj->Array.data[i]);
    }
  }
}


// ===== GC routines =====

static void memSwap(LeVM* vm) {
  vm->here = vm->old;
  vm->old  = vm->new;
  vm->new  = vm->here;
}

static Obj memforwarded(Obj obj) {
  assert(isMoved(obj->header));
  return (Obj)(obj->header);
}

static Obj memMove(LeVM* vm, Obj obj) {
  if (!le_is_obj(obj)) return obj;
  if (isMoved(obj->header)) return (Obj)obj->header;

  Cell cells  = cellsOf(obj->header);
  Cell actual = cells + OBJECT_HEADER_CELLS;
  Obj  src    = obj;
  Obj  dst    = (Obj)vm->here;
  vm->here += actual;
  
  for (int i = 0; i < actual; i++) {
    dst[i] = src[i];
  }

  obj->header = (Cell)dst;

  return dst;
}


// ===== GC entry point =====

Public void le_gc(LeVM* vm) {
  if (!vm->root) DIE("GC: no root");
  memSwap(vm);

  // move root
  vm->root = memMove(vm, vm->root);

  // move temporary stack
  for (int i = 0; i < vm->tp; i++) {
    vm->tmp[i] = memMove(vm, vm->tmp[i]);
  }

  // interpreter
  vm->env       = memMove(vm, vm->env);
  vm->result    = memMove(vm, vm->result);
  vm->err       = memMove(vm, vm->err);

  // Symbol Table
  for (int i = 0; i < SymTableSize; i++) {
    vm->symTable[i] = memMove(vm, vm->symTable[i]);
  }

  // scan loop
  vm->scanned = vm->new;
  while (vm->scanned < vm->here) {
    Obj  obj    = (Obj)vm->scanned;
    Cell cells  = cellsOf(obj->header);
    Cell actual = cells + OBJECT_HEADER_CELLS;
    vm->scanned += actual;

    // no need to scan
    if (isBytes(obj->header)) continue;

    // move contents
    for (int i = 0; i < cells; i++) {
      Obj x = obj->Array.data[i];
      obj->Array.data[i] = memMove(vm, obj->Array.data[i]);
    }
  }
}


// ===== Allot =====

static int memClaim(LeVM* vm, Cell cells) {
  // return 1 if there is not enough memory
  Obj* max  = vm->new + vm->cells;
  Cell diff = max - vm->here; // by cells
  return diff <= cells;
}

static Obj memAllot(LeVM* vm, Cell cells, Cell header) {
  Cell actual = cells + OBJECT_HEADER_CELLS;

  if (memClaim(vm, actual)) {
    le_gc(vm);
    if (memClaim(vm, actual)) DIE("LeVM: memory exhausted");
  }

  Obj obj = (Obj)vm->here;
  vm->here += actual;
  obj->header = header;
  obj->hash = nil;
  memset(&(obj->Array.data), nil, cells * sizeof(Cell)); // nil clear
  return obj;
}

static Obj newObj(LeVM* vm, Cell cells, int type) {
  Cell header = makeHeader(cells, type);
  return memAllot(vm, cells, header);
}

static LeObj* newBytesObj(LeVM* vm, int bytes, int type) {
  Cell cells = align(bytes) / sizeof(Cell) + 1; // size cell
  Obj b = newObj(vm, cells, type);
  b->Bytes.size = bytes;
  return b;
}


// Data Types
// =============================================================================

// ===== Root =====

#define ROOT_SIZE OBJ_SIZE(Root)

static Obj newRoot(LeVM* vm) {
  return newObj(vm, ROOT_SIZE, T_ARRAY);
}


// ===== Array =====

Public Obj le_new_array(LeVM* vm, int len) {
  return newObj(vm, len, T_ARRAY);
}

Public int le_array_len(Obj xs) {
  return cellsOf(xs->header);
}


// ===== Pair =====

Public Obj le_cons(LeVM* vm, Obj a, Obj b) {
  Push(b);
  Push(a);
  Obj xs = newObj(vm, 2, T_PAIR);
  xs->Pair.car = Pop();
  xs->Pair.cdr = Pop();
  return xs;
}

Public Obj le_car(Obj xs) {
  if (xs == nil) return nil;
  return xs->Pair.car;
}

Public Obj le_cdr(Obj xs) {
  if (xs == nil) return nil;
  return xs->Pair.cdr;
}

Public void le_set_car(Obj xs, Obj x) {
  xs->Pair.car = x;
}

Public void le_set_cdr(Obj xs, Obj x) {
  xs->Pair.cdr = x;
}

Public Obj le_second(Obj xs) {
  return Car(Cdr(xs));
}

Public Obj le_third(Obj xs)  {
  return Car(Cdr(Cdr(xs)));
}

Public Obj le_reverse_inplace(Obj xs) {
  Obj last = nil;

  while (xs != nil) {
    Obj rest = Cdr(xs);
    SetCdr(xs, last);
    last = xs;
    xs = rest;
  }

  return last;
}


// ===== String =====

Public Obj le_new_str(LeVM* vm, int len) {
  int actual = len+1; // null termination
  return newBytesObj(vm, actual, T_STRING);
}

Public Obj le_new_str_from(LeVM* vm, char* str) {
  int len = strlen(str);
  Obj s = le_new_str(vm, len);
  memcpy(s->Bytes.data, str, len+1);
  return s;
}

Public char* le_cstr_of(Obj str) {
  // returns NULL if str is not a string
  if (le_typeof(str) != Le_string) return NULL;
  return str->Bytes.data;
}

Public int le_str_len(Obj s) {
  // returns -1 if s is not a string
  if (le_typeof(s) != Le_string) return -1;
  return s->Bytes.size - 1; // null terminated
}

Public char le_str_get(Obj s, int i) {
  // No bound and type check, be careful
  return (char)s->Bytes.data[i];
}

Public Obj le_str_concat(LeVM* vm, Obj a, Obj b) {
  // returns a+b
  int len_a = le_str_len(a);
  int len_b = le_str_len(b);
  int len   = len_a + len_b;
  Push(a);
  Push(b);
  Obj s = le_new_str(vm, len);
  char* pb = le_cstr_of(Pop());
  char* pa = le_cstr_of(Pop());
  char* ps = le_cstr_of(s);
  memcpy(ps,         pa, len_a);
  memcpy(ps + len_a, pb, len_b);
  ps[len] = '\0';
  return s;
}

Public int le_str_eq(Obj a, Obj b) {
  if (a->hash != nil && b->hash != nil && a->hash != b->hash) return 0;
  char* sa = le_cstr_of(a);
  char* sb = le_cstr_of(b);
  return strcmp(sa, sb) == 0;
}


// ===== Bytes =====

Public Obj le_new_bytes(LeVM* vm, int len) {
  return newBytesObj(vm, len, T_BYTES);
}

Public int le_bytes_len(Obj xs) {
  return xs->Bytes.size;
}


// ===== Symbol =====
#define SYM_SIZE OBJ_SIZE(Symbol)

static char* symNameStr(Obj sym) {
  Obj s = sym->Symbol.name;
  return s->Bytes.data;
}

static Obj findSym(LeVM* vm, char* s) {
  // returns entry for get/set
  Obj xs = vm->root->Root.symbols;

  while (xs != nil) {
    if (strcmp(s, symNameStr(Car(xs))) == 0) return xs;
    xs = Cdr(xs);
  }
  return nil;
}

static void addSym(LeVM* vm, Obj sym) {
  // symtable will be traced in GC. no need to push/pop
  Obj xs = vm->root->Root.symbols;
  xs = Cons(vm, sym, xs);
  vm->root->Root.symbols = xs;
}

static Obj createSym(LeVM* vm, Obj name) {
  Push(name);
  Obj sym = newObj(vm, SYM_SIZE, T_SYMBOL);
  sym->Symbol.name = Pop();
  addSym(vm, sym);
  return sym;
}

Public Obj le_new_sym(LeVM* vm, Obj name) {
  // name: string
  Obj old = findSym(vm, le_cstr_of(name));
  if (old) return Car(old);
  
  return createSym(vm, name);
}

Public Obj le_new_sym_from(LeVM* vm, char* name) {
  Obj old = findSym(vm, name);
  if (old) return Car(old);

  Obj str = le_new_str_from(vm, name);
  return createSym(vm, str);
}


// ===== Func =====

#define FUNC_SIZE OBJ_SIZE(Func)

Public Obj le_new_func(LeVM* vm, Obj code, Obj env, Obj vars) {
  Push(code);
  Push(env);
  Push(vars);
  Obj c = newObj(vm, FUNC_SIZE, T_FUNC);
  c->Func.vars = Pop();  
  c->Func.env  = Pop();
  c->Func.code = Pop();
  return c;
}


// String utilities
// =============================================================================
// かんたんなdynamic string

typedef struct {
  char* s;
  char* p;
  int   len;  // contains null termination
  int   size;
} Str;

static void initStr(Str* s) {
  s->len  = 1;
  s->size = 64;
  s->s    = calloc(sizeof(char), s->size);
  s->p    = s->s;
}

static char* extractStr(Str* s) {
  int   len = s->len - 1;
  s->s[len] = '\0';
  return s->s;
}

static void claimStr(Str* s, int n) {
  s->len += n;
  if (s->len < s->size) return;

  s->size *= 2;
  int i = s->p - s->s;
  s->s = realloc(s->s, s->size);
  s->p = s->s + i;
}

static void putStr(Str* s, char* str) {
  int len = strlen(str);
  claimStr(s, len);
  memcpy(s->p, str, len);
  s->p += len;
}

static void putChar(Str* s, char c) {
  claimStr(s, 1);
  *s->p = c;
  s->p++;
}

static void putEscape(Str* s, char c) {
  putChar(s, '\\');
  putChar(s, c);
}


// Stringify
// =============================================================================
static void toStrSub(Str* s, Obj x);

static void toStrListBody(Str* s, Obj x) {
  int t = le_typeof(x);

  // end of list
  if (t == Le_nil) return;

  // dotted pair
  if (t != Le_pair) {
    putStr(s, " . ");
    toStrSub(s, x);
    return;
  }

  putStr(s, " ");
  toStrSub(s, Car(x));
  toStrListBody(s, Cdr(x));
}

static void toStrStr(Str* s, Obj x) {
  // unescape
  char* p = le_cstr_of(x);
  for (char c = *p; c != '\0'; c = *(++p)) {
    if (c == '\n') { putEscape(s, 'n'); continue; }
    if (c == '"')  { putEscape(s, '"'); continue; }
    putChar(s, c);
  }
}

static void toStrArrayBody(Str* s, Obj xs) {
  int len = le_array_len(xs);
  if (len == 0) return;

  toStrSub(s, xs->Array.data[0]);
  
  for (int i = 1; i < len; i++) {
    putChar(s, ' ');
    Obj x = xs->Array.data[i];
    toStrSub(s, x);
  }
}

static void unknownToStr(Str* s, Obj x) {
  DIE("toStr:unknown %p isObj %d ('%s')", x, le_is_obj(x), extractStr(s));  
}

static void toStrSub(Str* s, Obj x) {
  int t = le_typeof(x);
  switch (t) {
  case Le_nil:
    return putStr(s, "()");
  case Le_int:
    { 
      char buf[1024];
      snprintf(buf, 1024, "%ld", le_obj2int(x));
      return putStr(s, buf);
    }
  case Le_symbol:
    return putStr(s, symNameStr(x));
  case Le_pair:
    putStr(s, "(");
    toStrSub(s, Car(x));
    toStrListBody(s, Cdr(x));
    return putStr(s, ")");
  case Le_string:
    putStr(s, "\"");
    toStrStr(s, x);
    return putStr(s, "\"");
  case Le_func:
    return putStr(s, "#<Func>");
  case Le_array:
    putStr(s, "#<Array ");
    toStrSub(s, le_int2obj(le_array_len(x)));
    return putStr(s, ">");
  case Le_bytes:
    putStr(s, "#<Bytes ");
    toStrSub(s, le_int2obj(le_bytes_len(x)));
    return putStr(s, ">");
  }

  if (t == Le_unknown) unknownToStr(s, x);

  char buf[1024];
  snprintf(buf, 1024, "#<Unimplemented toStr for %d>", t);
  return putStr(s, buf);
}

static char* toStr(Obj x) {
  // note: you should free returned string
  Str s;
  initStr(&s);

  if (x == nil) {
    putStr(&s, "nil"); // toplevel nil
  } else {
    toStrSub(&s, x);
  }

  return extractStr(&s);
}

Public char* le_to_str(Obj x) {
  return toStr(x);
}

Public char* le_err_str(LeVM* vm) {
  return le_to_str(vm->err);
}


// VM
// =============================================================================

Public int le_vm_cells(LeVM* vm) {
  return vm->cells;
}

Public Obj le_vm_result(LeVM* vm) {
  return vm->result;
}

Public Obj le_vm_error(LeVM* vm) {
  return vm->err;
}


// ===== Global =====

#define GetSymValue(name) (Car(Sym(name)->Symbol.global))

static Obj findGlobalRef(LeVM* vm, Obj sym) {
  return sym->Symbol.global;
}

static Obj fetchGlobalRef(LeVM* vm, Obj sym) {
  // found: returns (sym . val)
  //   not: creates and returns (sym . nil)
  Obj ref = findGlobalRef(vm, sym);

  // found
  if (ref != nil) return ref;
  
  // not found
  ref = Cons(vm, sym, nil);
  sym->Symbol.global = ref;
  return ref;
}

static void setGlobal(LeVM* vm, Obj var, Obj val) {
  Push(val);
  Obj ref = fetchGlobalRef(vm, var);
  val = Pop();
  SetCar(ref, val);
}


// ===== Create VM =====

Public LeVM* le_new_vm(int cells, int repl_buf_size) {
  LeVM* vm = calloc(sizeof(LeVM), 1);

  vm->replBufSize = repl_buf_size;

  // memory
  vm->cells = cells;
  vm->new   = calloc(sizeof(Cell), cells);
  vm->old   = calloc(sizeof(Cell), cells);
  vm->here  = vm->new;
  vm->tmp   = calloc(sizeof(Obj), TEMPORARY_STACK_SIZE);

  // root
  vm->root = newRoot(vm);

  setupVM(vm);

  return vm;
}

Public LeVM* le_create_vm() {
  return le_new_vm(VM_DEFAULT_CELLS, VM_DEFAULT_REPL_BUF_SIZE);
}

Public void le_free_vm(LeVM* vm) {
  // Memory
  free(vm->new);
  free(vm->old);
  free(vm->tmp);

  free(vm);
}


// ===== VM Errors =====

Public int le_raise(LeVM* vm, Obj err) {
  vm->result = nil; // clean up
  vm->err = err;
  return Le_ERR;
}

Public int le_raise_with(LeVM* vm, Obj error, Obj x) {
  Push(error);
  Obj xs = Cons(vm, x, nil);
  xs = Cons(vm, Pop(), xs);
  Obj sym = Sym(Error);
  xs = Cons(vm, sym, xs);
  return le_raise(vm, xs);
}


// =============================================================================
// sxp reader
// =============================================================================

static int isDigit(char c) {
  return '0' <= c && c <= '9';
}

static int isBlank(char c) {
  return (c == ' ' || c == '\n' || c == '\t');
}

static int isDelimiter(char c) {
  return c == '(' || c == ')';
}

static int skipComment(LeVM* vm) {
  if (*vm->p != ';') return 0;
  vm->p++;
  while (*vm->p && *vm->p != '\n') { vm->p++; }
  if (!*vm->p) vm->p--; // rollback to before NULL for skipSpaces
  return 1;
}

static void skipSpaces(LeVM* vm) {
  while (isBlank(*vm->p) || skipComment(vm)) {
    vm->p++;
  }
}

static int readSym(LeVM* vm) {
  char* start = vm->p;
  for ( char c = *vm->p;
        c && !isBlank(c) && !isDelimiter(c);
        c = *(++vm->p)) {}

  int  len  = vm->p - start;
  char buf[1024];
  memcpy(buf, start, len);
  buf[len] = '\0';

  vm->result = le_new_sym_from(vm, buf);
  return Le_OK;
}

static int readNum(LeVM* vm) {
  int sign = 1;
  int acc  = 0;

  // handling sign or symbol
  if (vm->p[0] == '-') {
    if (!isDigit(vm->p[1])) return readSym(vm);
    sign = -1;
    vm->p++;
  }

  while (isDigit(*vm->p)) {
    acc = acc*10 + (*vm->p - '0');
    vm->p++;
  }

  vm->result = le_int2obj(acc*sign);
  return Le_OK;
}

// for recursion
static int readExpr(LeVM* vm);

static int readListBody(LeVM* vm) {
  skipSpaces(vm);

  char c = *vm->p;
  
  // Unclosed list
  if (c == '\0') return Le_Continue;

  // dot pair
  if (c == '.') {
    vm->p++;
    int code = readExpr(vm);
    ExpectOK;
    skipSpaces(vm);
    if (!*vm->p || *vm->p != ')') return Le_Continue;
    vm->p++;
    return Le_OK; // vm->result holds cdr
  }
  
  // end of list
  if (c == ')') {
    vm->p++;
    vm->result = nil;
    return Le_OK;
  }
  
  int code = readExpr(vm);
  ExpectOK;
  Push(vm->result);
  code = readListBody(vm);
  ExpectOK;
  vm->result = Cons(vm, Pop(), vm->result);
  
  return Le_OK;
}

static int readList(LeVM* vm) {
  vm->p++; // skip '('
  skipSpaces(vm);

  char c = *vm->p;

  // nil list
  if (c == ')') {
    vm->p++;
    vm->result = nil;
    return Le_OK;
  }

  // continue
  if (c == '\0') return Le_Continue;

  int code = readExpr(vm);
  ExpectOK;
  Push(vm->result);
  
  code = readListBody(vm);
  ExpectOK;
  
  vm->result = Cons(vm, Pop(), vm->result);
  return Le_OK;
}

static int readStr(LeVM* vm) {
  Str s;
  initStr(&s);
  
  vm->p++; // skip left "
  for (char c = *vm->p; c && c != '"'; c = *(++vm->p)) {
    if (c == '\0') return Le_Continue;
    
    if (c != '\\') {
      putChar(&s, c);
      continue;
    }
    
    // escape sequence
    c = *(++vm->p);
    switch (c) {
    case 'n': putChar(&s, '\n'); break;
    case 'r': putChar(&s, '\r'); break;
    default:  putChar(&s, c);    break;
    }
  }
  vm->p++; // skip right "
  
  char* str = extractStr(&s);
  Obj x = le_new_str_from(vm, str);
  free(str);
  vm->result = x;
  return Le_OK;
}

static int readQuote(LeVM* vm) {
  vm->p++; // skip quote
  int code = readExpr(vm);
  ExpectOK;

  vm->result = Cons(vm, Sym(Quote), vm->result);
  return Le_OK;
}

static int readQuasiquote(LeVM* vm) {
  vm->p++; // skip quasiquote
  int code = readExpr(vm);
  ExpectOK;

  vm->result = Cons(vm, Sym(Quasiquote), vm->result);
  return Le_OK;
}

static int readUnquote(LeVM* vm) {
  vm->p++; // skip unquote
  int splicing = *vm->p == '@';
  if (splicing) vm->p++; // skip @

  int code = readExpr(vm);
  ExpectOK;

  Obj sym = splicing ? Sym(UnquoteSplicing) : Sym(Unquote);
  vm->result = Cons(vm, sym, vm->result);
  return Le_OK;
}

static int readExpr(LeVM* vm) {
  // vm->p should be set
  
  skipSpaces(vm);
  char c = *vm->p;

  if (c == '\0') return Le_EOF;
  
  if (isDigit(c) || c == '-') return readNum(vm);

  if (c == '(')  return readList(vm);
  if (c == '"')  return readStr(vm);
  if (c == '\'') return readQuote(vm);
  if (c == '`')  return readQuasiquote(vm);
  if (c == ',')  return readUnquote(vm);

  return readSym(vm);
}

Public int le_read_str(LeVM* vm, char* src) {
  vm->src = src;
  vm->p   = src;
  return readExpr(vm);
}


// Eval
// =============================================================================
static int eval(LeVM* vm, Obj expr);

// ===== Local Env ====

static Obj findLocalRef(Obj env, Obj sym) {
  // found: (var . val)
  // not found: nil
  while (env != nil) {
    Obj binds = Car(env);
    while (binds != nil) {
      Obj bind = Car(binds);
      if (Car(bind) == sym) return bind;
      binds = Cdr(binds);
    }
    env = Cdr(env);
  }
  return nil;
}

static int findLocal(LeVM* vm, Obj env, Obj sym) {
  // found: vm->result = val, return Le_OK
  // not found: return Le_ERR
  Obj bind = findLocalRef(env, sym);
  if (bind == nil) return Le_ERR;
  vm->result = Cdr(bind);
  return Le_OK;
}


// ===== Syntax =====

static int evalExprs(LeVM* vm, Obj xs) {
  SaveStack;
  // exprs should be a pair or nil
  int code = Le_OK;

  vm->result = nil;
  while (xs != nil) {
    Obj expr = Car(xs);
    Push(Cdr(xs));
    code = eval(vm, expr); // vm->result holds last result
    if (code != Le_OK) RestoreReturn(code);

    xs = Pop();
  }
  
  // again, vm->result holds last result
  return Le_OK;
}

static int evalLet(LeVM* vm, Obj rest) {
  // (let ((var val) ...) expr ...)
  // env: ( ((var . val) ...) ... )
  SaveStack;
  int env_i = Push(vm->env);
  Obj binds = Car(rest);
  Obj body = Cdr(rest);
  Push(body);
  int code = Le_OK;

  vm->env = le_cons(vm, nil, vm->env);
  while (binds) {
    Push(binds);
    Obj bind = Car(binds);
    Obj var = Car(bind);
    Obj val = Second(bind);
    Push(var);
    code = eval(vm, val);
    if (code != Le_OK) {
      vm->env = le_stack_at(vm, env_i);
      RestoreReturn(code);
    }

    val = vm->result;
    var = Pop();
    // create bind var/val on env, as dotted pair
    bind = le_cons(vm, var, val);
    Obj env = le_cons(vm, bind, Car(vm->env));
    SetCar(vm->env, env);
    // next
    binds = Cdr(Pop());
  }
  
  body = Pop();
  code = evalExprs(vm, body);
  // result or err is set on vm
  vm->env = Pop();
  
  return code;
}

static int evalFn(LeVM* vm, Obj rest) {
  if (!le_is_pair(rest)) return RaiseWith(MalformedFn, rest);
  Obj binds = Car(rest);
  Obj body = Cdr(rest);
  if (binds != nil && !le_is_pair(binds) && !le_is_symbol(binds))
    return RaiseWith(MalformedFn, rest);

  Obj f = le_new_func(vm, body, vm->env, binds);
  vm->result = f;
  return Le_OK;
}

static int evalArgs(LeVM* vm, Obj args) {
  SaveStack;
  if (args == nil) {
    vm->result = nil;
    return Le_OK;
  }
  
  if (!le_is_pair(args)) return RaiseWith(InvalidArgs, args);

  Obj vals = nil;
  for (; args; args = Cdr(args)) {
    Obj expr = Car(args);
    Push(args);
    Push(vals);
    int code = eval(vm, expr);
    if (code != Le_OK) RestoreReturn(code);
    Obj val = vm->result;
    vals = Pop();
    vals = le_cons(vm, val, vals);
    args = Pop();
  }

  vm->result = le_reverse_inplace(vals);
  return Le_OK;
}

static int applyFunc(LeVM* vm, Obj func, Obj args) {
  // func should be type checked

  // swap env
  Push(vm->env);
  vm->env = func->Func.env;
  
  // create env
  Push(func);
  Obj vars = func->Func.vars;
  Obj env = nil;
  while (vars != nil) {
    // rest
    if (le_is_symbol(vars)) {
      Push(env);
      Obj bind = le_cons(vm, vars, args);
      env = le_cons(vm, bind, Pop());
      break;
    }

    // single
    Push(vars);
    Push(args);
    Push(env);
    Obj bind = le_cons(vm, Car(vars), Car(args));
    env = le_cons(vm, bind, Pop());
    args = Cdr(Pop());
    vars = Cdr(Pop());
  }
  vm->env = le_cons(vm, env, vm->env);
  func = Pop();

  // apply
  int code = evalExprs(vm, func->Func.code);
  // result or err is set on vm
  
  // restore env
  vm->env = Pop();

  return code;
}

static int evalDef(LeVM* vm, Obj xs) {
  SaveStack;
  // xs: (var val)
  Obj var = Car(xs);
  Obj val = Second(xs);
  Push(var);
  int code = eval(vm, val);
  if (code != Le_OK) RestoreReturn(code);
  val = vm->result; // also this is return value
  var = Pop();
  setGlobal(vm, var, val);
  return Le_OK;
}

static int evalIf(LeVM* vm, Obj xs) {
  SaveStack;
  Obj cond = Car(xs);
  Obj then = Second(xs);
  Obj els  = Third(xs);
  Push(els);
  Push(then);
  int code = eval(vm, cond);
  if (code != Le_OK) RestoreReturn(code);

  then = Pop(); // then
  els  = Pop();
  Obj expr = vm->result != nil ? then : els;
  return eval(vm, expr);
}

static int evalSet(LeVM* vm, Obj xs) {
  SaveStack;
  Obj var = Car(xs);
  Obj val = Second(xs);
  if (!le_is_symbol(var))
    return RaiseWith(InvalidArgs, xs);

  // eval value
  Push(var);
  int code = eval(vm, val);
  if (code != Le_OK) RestoreReturn(code);
  
  var = Pop();

  // search local
  Obj ref = findLocalRef(vm->env, var);
  if (ref != nil) {
    SetCdr(ref, vm->result);
    return Le_OK;
  }

  // search global
  ref = findGlobalRef(vm, var);
  if (ref != nil) {
    SetCar(ref, vm->result);
    return Le_OK;
  }

  return RaiseWith(UndefinedSymbol, var);
}

static int evalWhile(LeVM* vm, Obj xs) {
  SaveStack;
  Obj cond = Car(xs);
  Obj body = Cdr(xs);
 
  int cond_i = Push(cond);
  int body_i = Push(body);
  int code = eval(vm, cond);
  while (code == Le_OK && vm->result != nil) {
    body = le_stack_at(vm, body_i);
    code = evalExprs(vm, body);
    if (code == Le_Continue) code = Le_OK;
    if (code == Le_Break)    break;
    if (code != Le_OK) RestoreReturn(code);
    cond = le_stack_at(vm, cond_i);
    code = eval(vm, cond);
  }

  vm->result = nil;
  RestoreReturn(Le_OK);
}

static int evalCatch(LeVM* vm, Obj xs) {
  // (catch expr fn)
  SaveStack;
  Obj expr = Car(xs);
  Obj f = Second(xs);
  Push(f);

  int code = eval(vm, expr);
  if (code != Le_ERR) RestoreReturn(code);

  // raised
  f = Pop();
  Push(vm->err);
  code = eval(vm, f);
  if (code != Le_OK) RestoreReturn(code);
  
  f = vm->result;
  if (!le_is_func(vm->result))
    RestoreReturn(RaiseWith(InvalidArgs, f));

  Obj err = Pop();
  Push(f);
  Obj args = Cons(vm, err, nil);
  f = Pop();

  return applyFunc(vm, f, args);
}

static int evalQuote(LeVM* vm, Obj xs) {
  vm->result = xs;
  return Le_OK;
}

static int evalApply(LeVM* vm, Obj xs) {
  // (apply F Args)
  // F and Args should be evaled
  Obj f    = Car(xs);
  Obj args = Second(xs);
  if (!le_is_func(f))
    return RaiseWith(NotAProc, f);
  return applyFunc(vm, f, args);
}


// Primitive Handlers
// =============================================================================
// SymPrimAddならevalPrimAddというように、必ずevalPrimNameという名前にすること

// SymbolTableのサイズを利用。少し大きくなるけどコードを簡単にしたいから仕方ない
typedef int(*PrimHandler)(LeVM* vm, Obj args);
PrimHandler PrimitiveTable[SymTableSize];


// ===== Arithmetics =====

#define ARITH_PROLOGUE                                                  \
  SaveStack;                                                            \
  Obj a = Car(args);                                                    \
  Obj b = Second(args);                                                 \
  if (!le_is_num(a) || !le_is_num(b))                                   \
    return RaiseWith(ExpectInteger, args);                              \
  int na = le_obj2int(a);                                               \
  int nb = le_obj2int(b);                                             


static int evalPrimAdd(LeVM* vm, Obj args) {
  ARITH_PROLOGUE;
  Obj r = le_int2obj(na + nb);
  vm->result = r;
  return Le_OK;
}

static int evalPrimSub(LeVM* vm, Obj args) {
  ARITH_PROLOGUE;
  Obj r = le_int2obj(na - nb);
  vm->result = r;
  return Le_OK;
}

static int evalPrimMul(LeVM* vm, Obj args) {
  ARITH_PROLOGUE;
  Obj r = le_int2obj(na * nb);
  vm->result = r;
  return Le_OK;
}

static int evalPrimDiv(LeVM* vm, Obj args) {
  ARITH_PROLOGUE;

  if (nb == 0)
    return RaiseWith(ZeroDivision, args);

  Obj r = le_int2obj(na / nb);
  vm->result = r;
  return Le_OK;
}

static int evalPrimMod(LeVM* vm, Obj args) {
  ARITH_PROLOGUE;

  if (nb == 0)
    return RaiseWith(ZeroDivision, args);

  Obj r = le_int2obj(na % nb);
  vm->result = r;
  return Le_OK;
}


// ===== Compare =====

static int evalPrimEq(LeVM* vm, Obj args) {
  SaveStack;
  Obj a = Car(args);
  Obj b = Second(args);

  vm->result = a == b ? Sym(True) : nil;
  return Le_OK;
}

static int evalPrimNot(LeVM* vm, Obj args) {
  Obj x = Car(args);
  vm->result = x == nil ? Sym(True) : nil;
  return Le_OK;
}

static int evalPrimGt(LeVM* vm, Obj args) {
  SaveStack;
  Obj a = Car(args);
  Obj b = Second(args);

  if (!le_is_num(a) || !le_is_num(b))
    return RaiseWith(ExpectInteger, args);
  int na = le_obj2int(a);
  int nb = le_obj2int(b);
  vm->result = na > nb ? Sym(True) : nil;
  return Le_OK;
}


// ===== Types =====

static int evalPrimTypeOf(LeVM* vm, Obj args) {
  // (%prim:type-of Obj) => Symbol
  Obj x = Car(args);
  int ty = le_typeof(x);
  Obj r = nil;
  switch (ty) {
  case Le_nil:     r = Sym(Nil); break;
  case Le_int:     r = Sym(Number); break;
  case Le_array:   r = Sym(Array); break;
  case Le_symbol:  r = Sym(Symbol); break;
  case Le_pair:    r = Sym(Pair); break;
  case Le_func:    r = Sym(Func); break;
  case Le_bytes:   r = Sym(Bytes); break;
  case Le_string:  r = Sym(String); break;
  }
  vm->result = r;
  return Le_OK;
}

static int evalPrimHash(LeVM* vm, Obj args) {
  Obj x = Car(args);
  vm->result = le_get_hash(x);
  return Le_OK;
}


// ===== Array =====

static int evalPrimArrayNew(LeVM* vm, Obj args) {
  Obj x = Car(args);
  if (!le_is_num(x))
    return RaiseWith(InvalidArgs, args);
  int len = le_obj2int(x);
  vm->result = le_new_array(vm, len);
  return Le_OK;
}

static int evalPrimArrayGet(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj obj_i  = Second(args);
  if (!(le_is_array(xs) && le_is_num(obj_i)))
    return RaiseWith(InvalidArgs, args);
  int i = le_obj2int(obj_i);
  if (i < 0 || i >= le_array_len(xs))
    return RaiseWith(OutOfRange, args);
  vm->result = xs->Array.data[i];
  return Le_OK;
}

static int evalPrimArraySet(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj obj_i  = Second(args);
  Obj val = Third(args);
  if (!(le_is_array(xs) && le_is_num(obj_i)))
    return RaiseWith(InvalidArgs, args);
  int i = le_obj2int(obj_i);
  if (i < 0 || i >= le_array_len(xs))
    return RaiseWith(OutOfRange, args);  
  xs->Array.data[i] = val;
  vm->result = xs;
  return Le_OK;
}

static int evalPrimArrayLen(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  if (!le_is_array(xs))
    return RaiseWith(InvalidArgs, args);
  vm->result = le_int2obj(le_array_len(xs));
  return Le_OK;
}


// ===== Pair =====

static int evalPrimCons(LeVM* vm, Obj args) {
  vm->result = Cons(vm, Car(args), Second(args));
  return Le_OK;
}

static int evalPrimCar(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  if (xs != nil && !le_is_pair(xs))
    return RaiseWith(InvalidArgs, args);
  vm->result = Car(xs);
  return Le_OK;
}

static int evalPrimCdr(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  if (xs != nil && !le_is_pair(xs))
    return RaiseWith(InvalidArgs, args);
  vm->result = Cdr(xs);
  return Le_OK;
}

static int evalPrimSetCar(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj x  = Second(args);
  if (xs != nil && !le_is_pair(xs))
    return RaiseWith(InvalidArgs, args);
  SetCar(xs, x);
  vm->result = xs;
  return Le_OK;
}

static int evalPrimSetCdr(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj x  = Second(args);
  if (xs != nil && !le_is_pair(xs))
    return RaiseWith(InvalidArgs, args);
  SetCdr(xs, x);
  vm->result = xs;
  return Le_OK;
}


// ===== Symbol =====

static int evalPrimSymNew(LeVM* vm, Obj args) {
  // (%prim:sym-new Str) => Symbol
  Obj name = Car(args);
  if (!le_is_string(name))
    return RaiseWith(InvalidArgs, args);
  char* s = le_cstr_of(name);
  vm->result = le_new_sym_from(vm, s);
  return Le_OK;
}

static int evalPrimSymStr(LeVM* vm, Obj args) {
  // (%prim:sym-str Sym) => String
  Obj sym = Car(args);
  if (!le_is_symbol(sym))
    return RaiseWith(InvalidArgs, args);
  vm->result = sym->Symbol.name;
  return Le_OK;
}


// ===== String =====

static int evalPrimStr(LeVM* vm, Obj args) {
  // (%prim:str Obj) => String
  Obj x = Car(args);
  char* s = toStr(x);
  vm->result = le_new_str_from(vm, s);
  free(s);
  return Le_OK;
}

static int evalPrimStrEq(LeVM* vm, Obj args) {
  // (%prim:str-eq A B) => true/nil
  Obj a = Car(args);
  Obj b = Second(args);
  if (!(le_is_string(a) && le_is_string(b)))
    return RaiseWith(InvalidArgs, args);
  vm->result = le_str_eq(a, b) ? Sym(True) : nil;
  return Le_OK;
}

static int evalPrimStrCat(LeVM* vm, Obj args) {
  // (%prim:str-cat ListOfStrings) => String
  SaveStack;
  Obj xs = Car(args);
  Push(xs);
  int size = 0;

  // アロケーションとコピー何度もやりたくないので、まず一回argsを見て
  // 合計サイズを先に出して文字列を作る
  while(xs != nil) {
    Obj s = Car(xs);
    if (!le_is_string(s))
      RestoreReturn(RaiseWith(InvalidArgs, args));
    size += le_str_len(s);
    xs = Cdr(xs);
  }

  Obj str = le_new_str(vm, size);
  char* p = str->Bytes.data;

  // 改めてコピー
  xs = Pop();
  while (xs != nil) {
    Obj s = Car(xs);
    int len = le_str_len(s);
    memcpy(p, s->Bytes.data, len);
    p += len;
    xs = Cdr(xs);
  }
  p[size] = '\0';
  vm->result = str;
  
  return Le_OK;
}

static int evalPrimStrLen(LeVM* vm, Obj args) {
  Obj str = Car(args);
  if (!le_is_string(str))
    return RaiseWith(InvalidArgs, args);
  
  int len = le_str_len(str);
  vm->result = le_int2obj(len);
  return Le_OK;
}

static int evalPrimStrGet(LeVM* vm, Obj args) {
  Obj str = Car(args);
  Obj obj_i = Second(args);
  if (!(le_is_string(str) && le_is_num(obj_i)))
    return RaiseWith(InvalidArgs, args);
  
  int i = le_obj2int(obj_i);
  if (i < 0 || i >= le_str_len(str))
    return RaiseWith(OutOfRange, args);
  
  char c = le_str_get(str, i);
  vm->result = le_int2obj(c);
  return Le_OK;
}

static int evalPrimStrMake(LeVM* vm, Obj args) {
  // (%prim:str-make ArrayOfChars) => String
  Obj xs = Car(args);
  if (!le_is_array(xs))
    return RaiseWith(InvalidArgs, args);
  int len = le_array_len(xs);
  Push(xs);
  Obj str = le_new_str(vm, len);
  xs = Pop();
  for (int i = 0; i < len; i++) {
    Obj c = xs->Array.data[i];
    if (!le_is_num(c))
      return RaiseWith(ExpectInteger, xs);
    str->Bytes.data[i] = (char)(le_obj2int(c));
  }
  str->Bytes.data[len] = '\0';
  vm->result = str;
  return Le_OK;
}


// ===== Bytes =====

static int evalPrimBytesNew(LeVM* vm, Obj args) {
  Obj x = Car(args);
  if (!le_is_num(x))
    return RaiseWith(ExpectInteger, args);
  int len = le_obj2int(x);
  vm->result = le_new_bytes(vm, len);
  return Le_OK;
}

static int evalPrimBytesGet(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj obj_i  = Second(args);
  if (!(le_is_bytes(xs) && le_is_num(obj_i)))
    return RaiseWith(InvalidArgs, args);
  int i = le_obj2int(obj_i);
  if (i < 0 || i >= le_bytes_len(xs))
    return RaiseWith(OutOfRange, args);
  
  vm->result = le_int2obj(xs->Bytes.data[i]);
  return Le_OK;
}

static int evalPrimBytesSet(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  Obj obj_i  = Second(args);
  Obj val = Third(args);
  if (!(le_is_bytes(xs) && le_is_num(obj_i) && le_is_num(val)))
    return RaiseWith(InvalidArgs, args);
  int i = le_obj2int(obj_i);

  if (i < 0 || i >= le_bytes_len(xs))
    return RaiseWith(OutOfRange, args);

  xs->Bytes.data[i] = (Byte)(le_obj2int(val));
  vm->result = xs;
  return Le_OK;
}

static int evalPrimBytesLen(LeVM* vm, Obj args) {
  Obj xs = Car(args);
  if (!le_is_bytes(xs))
    return RaiseWith(InvalidArgs, args);
  vm->result = le_int2obj(le_bytes_len(xs));
  return Le_OK;
}


// ===== Read =====

static int evalPrimReadStr(LeVM* vm, Obj args) {
  Obj s = Car(args);
  ExpectType(string, s);
  return le_read_str(vm, le_cstr_of(s)); // result holds expression
}


// ===== I/O =====

static int evalPrimPutc(LeVM* vm, Obj args) {
  // (%prim:putc FileDescriptor Char) => Char
  Obj var_fd   = Car(args);
  Obj var_char = Second(args);
  if (!(le_is_num(var_fd) && le_is_num(var_char)))
    return RaiseWith(InvalidArgs, args);

  int fd = le_obj2int(var_fd);
  char c = le_obj2int(var_char);
  FILE* fp = fdopen(fd, "w");
  if (fp == NULL)
    return RaiseWith(InvalidFileDescriptor, args);

  putc(c, fp);
  fflush(fp);
  vm->result = var_char;
  return Le_OK;
}

static int evalPrimGetc(LeVM* vm, Obj args) {
  // (%prim:getc FileDescriptor) => char(integer)
  Obj var_fd = Car(args);
  if (!le_is_num(var_fd))
    return RaiseWith(InvalidArgs, args);

  int fd = le_obj2int(var_fd);
  FILE* fp = fdopen(fd, "r");
  if (fp == NULL)
    return RaiseWith(InvalidFileDescriptor, args);

  char c = getc(fp);
  vm->result = le_int2obj(c);
  return Le_OK;
}

static int evalPrimPrint(LeVM* vm, Obj args) {
  // (%prim:print FileDescriptor String) => String
  Obj obj_fd = Car(args);
  Obj str    = Second(args);
  if (!(le_is_num(obj_fd) && le_is_string(str)))
    return RaiseWith(InvalidArgs, args);

  int fd = le_obj2int(obj_fd);
  FILE* fp = fdopen(fd, "w");
  if (fp == NULL)
    return RaiseWith(InvalidFileDescriptor, args);

  char* s = le_cstr_of(str);
  fprintf(fp, "%s", s);
  fflush(fp);
  
  vm->result = str;
  return Le_OK;
}

static int evalPrimReadTextFile(LeVM* vm, Obj args) {
  // (%prim:read-text-file FileName) => String
  Obj fname = Car(args);
  ExpectType(string, fname);
  char* text = readTextFile(le_cstr_of(fname));
  if (text == NULL)
    return le_raise_with(vm, Sym(FileNotFound), fname);
  vm->result = le_new_str_from(vm, text);
  free(text);
  return Le_OK;
}

static int evalPrimWriteTextFile(LeVM* vm, Obj args) {
  // (%prim:write-text-file FileName String) => String
  Obj fname = Car(args);
  Obj text = Second(args);
  ExpectType(string, text);
  ExpectType(string, fname);
  if (!writeTextFile(le_cstr_of(fname), le_cstr_of(text)))
    return RaiseWith(UnknownError, args);
  vm->result = text;
  return Le_OK;
}


// ===== Error =====

static int evalPrimRaise(LeVM* vm, Obj args) {
  // (%prim:raise Error) => raise with Error
  vm->err = Car(args);
  return Le_ERR;
}


// ===== System =====

static int evalPrimExit(LeVM* vm, Obj args) {
  // (%prim:exit Code) => exit with code
  Obj code = Car(args);

  if (!le_is_num(code))
    return RaiseWith(InvalidArgs, args);

  exit(le_obj2int(code));
  return Le_OK; // avoid warning
}

static int evalPrimGC(LeVM* vm, Obj args) {
  // (%prim:gc) => nil
  //TODO returns some information
  le_gc(vm);
  vm->result = nil;
  return Le_OK;
}

static int evalPrimLoadFile(LeVM* vm, Obj args) {
  Obj fname = Car(args);
  ExpectType(string, fname);
  return le_load_file(vm, le_cstr_of(fname));
}


// ===== Pair =====

static int evalPair(LeVM* vm, Obj xs) {
  SaveStack;
  Obj first = Car(xs);
  Obj rest  = Cdr(xs);
  int code;

  // Syntax
  if (first == Sym(Let))      return evalLet(vm, rest);
  if (first == Sym(Fn))       return evalFn(vm, rest);
  if (first == Sym(Def))      return evalDef(vm, rest);
  if (first == Sym(If))       return evalIf(vm, rest);
  if (first == Sym(Set))      return evalSet(vm, rest);
  if (first == Sym(While))    return evalWhile(vm, rest);
  if (first == Sym(Break))    return Le_Break;
  if (first == Sym(Continue)) return Le_Continue;
  if (first == Sym(Catch))    return evalCatch(vm, rest);
  if (first == Sym(Quote))    return evalQuote(vm, rest);

  // eval args
  Push(first);
  code = evalArgs(vm, rest);
  if (code != Le_OK) RestoreReturn(code);
  Obj args = vm->result;
  first = Pop();

  if (first == Sym(Apply))
    return evalApply(vm, args);

  // primitive
  if (le_is_symbol(first) && first->Symbol.prim != nil) {
    int i = le_obj2int(first->Symbol.prim);
    PrimHandler f = PrimitiveTable[i];
    return f(vm, args);
  }

  // eval f
  Push(args);
  code = eval(vm, first);
  ExpectOK;
  Obj f = vm->result;
  args = Pop();  

  if (le_is_func(f)) return applyFunc(vm, f, args);

  return RaiseWith(NotAProc, f);
}


// ===== Symbol =====

static int evalSymbol(LeVM* vm, Obj sym) {
  // binds: ( ((var . val) ...) ... )
  Obj env = vm->env;

  // local
  int code = findLocal(vm, env, sym);
  if (code == Le_OK) return Le_OK;

  // global
  Obj ref = findGlobalRef(vm, sym);
  if (ref != nil) {
    vm->result = Car(ref);
    return Le_OK;
  }

  return RaiseWith(UndefinedSymbol, sym);
}


// ===== Eval =====

static int eval(LeVM* vm, Obj expr) {
  if (expr == nil || le_is_num(expr) || le_is_string(expr)) {
    vm->result = expr;
    return Le_OK;
  }

  if (le_is_pair(expr)) {
    return evalPair(vm, expr);
  }

  if (le_is_symbol(expr)) {
    return evalSymbol(vm, expr);
  }

  DIE("Unimplemented eval for %s", toStr(expr));
}


// ===== Pre Eval =====

int preEval(LeVM* vm, Obj expr) {
  Obj proc = GetSymValue(PreEval);

  if (proc == nil) {
    vm->result = expr;
    return Le_OK;
  }

  if (!le_is_func(proc))
    return RaiseWith(InvalidPreEvalProc, proc);

  Obj args = Cons(vm, expr, nil);
  return applyFunc(vm, proc, args);
}


// ===== Eval API =====

Public int le_eval(LeVM* vm, Obj expr) {
  int code = preEval(vm, expr);
  ExpectOK;

  Obj expanded = vm->result;
  code = eval(vm, expanded);

  return code;
}

Public int le_eval_str(LeVM* vm, char* src) {
  int code = le_read_str(vm, src);
  ExpectOK;
  return le_eval(vm, vm->result);
}

Public int le_eval_all_str(LeVM* vm, char* src) {
  vm->src = src;
  vm->p   = src;
  
  int code = Le_OK;
  int i = 0;
  while (code == Le_OK && *vm->p != '\0') {
    code = readExpr(vm);
    if (code == Le_EOF) break;
    ExpectOK;
    code = le_eval(vm, vm->result);
    ExpectOK;
  }

  if (code == Le_EOF) return Le_OK;
  return code;
}

Public int le_load_file(LeVM* vm, char* fname) {
  char* src = readTextFile(fname);

  if (src == NULL) {
    Obj filename = le_new_str_from(vm, fname);
    return RaiseWith(FileNotFound, filename);
  }

  int code = le_eval_all_str(vm, src);
  free(src);
  return code;
}

Public int le_load_corelib(LeVM* vm) {
  return le_eval_all_str(vm, corelib_src);
}


// REPL
// =============================================================================

#define REPL_BUF_LEN 4096

static void replPrompt(LeVM* vm) {
  printf(";> ");
  fflush(stdout);
}

static int replGets(char* buf, int max) {
  buf = fgets(buf, max, stdin);
  if (buf == NULL) return -1;
  int len = strlen(buf);
  buf[len-1] = '\0'; // remove newline
  return len-1;
}

static int replRead(LeVM* vm, char* buf, int max) {
  char* p = buf;
  int len = replGets(p, max);
  if (len < 0) return Le_EOF; // Ctrl-D
  max = max - len;
  p += len;

  int code = le_read_str(vm, buf);
  if (code == Le_EOF) return Le_Continue; // 空白のみの入力だった
  
  while (code == Le_Continue) {
    if (max < 1) DIE("repl buffer exausted");
    
    printf("  ");
    fflush(stdout);

    len = replGets(p, max);
    max = max - len;
    p += len;

    code = le_read_str(vm, buf);
  }

  return code;
}

static void printError(Obj err) {
  fprintf(stderr, "\e[31m");
  fprintf(stderr, "%s", toStr(err));
  fprintf(stderr, "\e[0m\n");
}

Public int le_repl(LeVM* vm) {
  char* buf = calloc(sizeof(char), vm->replBufSize);

  while (1) {
    replPrompt(vm);

    int code = replRead(vm, buf, vm->replBufSize);
    if (code == Le_Continue) continue; // 空白のみの入力
    if (code == Le_EOF)      break;    // Ctrl-D 
    if (code != Le_OK) {
      DBG("%s", vm->err);
      continue;
    }

    Obj expr = vm->result;
    code = le_eval(vm, expr);
    if (code == Le_OK) {
      printf("%s\n", toStr(vm->result));
    } else {
      printError(vm->err);
    }
  }

  free(buf);
}


// VM Setup
// =============================================================================

#define DefSym(index, name) (vm->symTable[Sym##index] = le_new_sym_from(vm, name))

static void setupSymbols(LeVM* vm) {
  DefSym(Nil,                   "nil");
  DefSym(True,                  "true");
  DefSym(Quote,                 "quote");
  DefSym(Quasiquote,            "quasiquote");
  DefSym(Unquote,               "unquote");
  DefSym(UnquoteSplicing,       "unquote-splicing");
  DefSym(Let,                   "let");
  DefSym(Fn,                    "fn");
  DefSym(Def,                   "def");
  DefSym(If,                    "if");
  DefSym(Set,                   "set!");
  DefSym(While,                 "while");
  DefSym(Break,                 "break");
  DefSym(Continue,              "continue");
  DefSym(Apply,                 "apply");
  DefSym(Catch,                 "catch");
  DefSym(PreEval,               "%pre-eval");
  /* types */
  DefSym(Number,                "number");
  DefSym(Array,                 "array");
  DefSym(Symbol,                "symbol");
  DefSym(Pair,                  "pair");
  DefSym(Func,                  "func");
  DefSym(Bytes,                 "bytes");
  DefSym(String,                "string");  
  /* errors */
  DefSym(Error,                 "error");
  DefSym(UndefinedSymbol,       "undefined-symbol");
  DefSym(MalformedFn,           "malformed-fn");
  DefSym(InvalidArgs,           "invalid-args");
  DefSym(ExpectInteger,         "expect-integer");
  DefSym(ZeroDivision,          "zero-division");
  DefSym(FileNotFound,          "file-not-found");
  DefSym(InvalidPreEvalProc,    "invalid-pre-eval-proc");
  DefSym(InvalidFileDescriptor, "invalid-file-descriptor");
  DefSym(NotAProc,              "not-a-proc");
  DefSym(OutOfRange,            "out-of-range");
  DefSym(UnknownError,          "unknown-error");
}

#define DefPrim(index, name) {                                   \
    DefSym(Prim##index, "%prim:" name);                          \
    Sym(Prim##index)->Symbol.prim = le_int2obj(SymPrim##index);  \
    PrimitiveTable[SymPrim##index] = evalPrim##index;            \
  }

static void setupPrimitives(LeVM* vm) {
 /* Arithmetics */
  DefPrim(Add,               "add");
  DefPrim(Sub,               "sub");
  DefPrim(Mul,               "mul");
  DefPrim(Div,               "div");
  DefPrim(Mod,               "mod");
  /* Compare */
  DefPrim(Eq,                "eq");
  DefPrim(Not,               "not");
  DefPrim(Gt,                "gt");
  /* Types */
  DefPrim(TypeOf,            "type-of");
  DefPrim(Hash,              "hash");
  /* Array */
  DefPrim(ArrayNew,          "array-new");
  DefPrim(ArrayGet,          "array-get");
  DefPrim(ArraySet,          "array-set!");
  DefPrim(ArrayLen,          "array-len");
  /* Pair */
  DefPrim(Cons,              "cons");
  DefPrim(Car,               "car");
  DefPrim(Cdr,               "cdr");
  DefPrim(SetCar,            "set-car!");
  DefPrim(SetCdr,            "set-cdr!");
  /* Symbol */
  DefPrim(SymNew,            "sym-new");
  DefPrim(SymStr,            "sym-str");
  /* String */
  DefPrim(Str,               "str");
  DefPrim(StrLen,            "str-len");
  DefPrim(StrGet,            "str-get");
  DefPrim(StrEq,             "str-eq");
  DefPrim(StrCat,            "str-cat");
  DefPrim(StrMake,           "str-make");
  /* Bytes */
  DefPrim(BytesNew,          "bytes-new");
  DefPrim(BytesGet,          "bytes-get");
  DefPrim(BytesSet,          "bytes-set!");
  DefPrim(BytesLen,          "bytes-len");
  /* Error */
  DefPrim(Raise,             "raise");
  /* Read */
  DefPrim(ReadStr,           "read-str");
  /* I/O */
  DefPrim(Putc,              "putc");
  DefPrim(Getc,              "getc");
  DefPrim(Print,             "print");
  DefPrim(ReadTextFile,      "read-text-file");
  DefPrim(WriteTextFile,     "write-text-file");
  /* System */
  DefPrim(Exit,              "exit");
  DefPrim(GC,                "gc");
  DefPrim(LoadFile,          "load-file");  
}

#define SetAsIs(symname) (setGlobal(vm, Sym(symname), Sym(symname)))

static void setupVariables(LeVM* vm) {
  setGlobal(vm, Sym(Nil), nil);
  setGlobal(vm, Sym(PreEval), nil);
  SetAsIs(True);
  SetAsIs(Number);
  SetAsIs(Array);
  SetAsIs(Symbol);
  SetAsIs(Pair);
  SetAsIs(Func);
  SetAsIs(Bytes);
  SetAsIs(String);
}

static void setupVM(LeVM* vm) {
  setupSymbols(vm);
  setupPrimitives(vm);
  setupVariables(vm);
}
