#include "lesxe.c"

#define TEST_CORELIB_FILE "./src/core.le"

void test_align() {
  int s = sizeof(Cell);
  assert(align(0)  == s * 0);
  assert(align(1)  == s * 1);
  assert(align(7)  == s * 1);
  assert(align(8)  == s * 1);
  assert(align(16) == s * 2);
}


void test_tagged_pointer() {
  // premise of cell size
  assert(sizeof(Cell) == sizeof(LeObj*));

  // nil
  assert(le_is_nil(nil) && !le_is_num(nil) && !le_is_obj(nil));

  // number
  Cell n = -123;
  Obj p = le_int2obj(n);
  /* reverse    */ assert(n == le_obj2int(p));
  /* converted? */ assert((LeObj*)n != p);
  /* check type */ assert(le_is_num(p) && !le_is_nil(p) && !le_is_obj(p));

  // object
  Obj o = (LeObj*)&n;
  /* check type */ assert(le_is_obj(o) && !le_is_nil(p) && !le_is_num(o));
}


void test_object_header() {
  // サイズの符号の扱いをテスト。
  // headerに入れたときに符号フラグが立っているサイズを作り、
  // 符号なしとして取り出せるかチェックする。
  // cellsOfのキャストを消して失敗するか確認できる。
  Cell size = -1 << HEADER_CELLS_BIT;
  size = ((uintptr_t)size) >> HEADER_CELLS_BIT;
  assert(size > 0);

  Cell header = makeHeader(size, T_ARRAY);
  assert(!isBytes(header) && typeOf(header) == T_ARRAY);
  assert(size == cellsOf(header));


  // moved
  assert(!isMoved(header));
  assert(isMoved((Cell)&header));


  // set type
  Cell h; // header
  h = setType(h, T_SYMBOL);  assert(!isBytes(h) && typeOf(h) == T_SYMBOL);
  h = setType(h, T_PAIR);    assert(!isBytes(h) && typeOf(h) == T_PAIR);
  h = setType(h, T_FUNC);    assert(!isBytes(h) && typeOf(h) == T_FUNC);
  h = setType(h, T_USER);    assert(!isBytes(h) && typeOf(h) == T_USER);
  h = setType(h, T_BYTES);   assert(isBytes(h)  && typeOf(h) == T_BYTES);
  h = setType(h, T_STRING);  assert(isBytes(h)  && typeOf(h) == T_STRING);


  // set cells
  h = setCells(h, 1);    assert(cellsOf(h) == 1);
  h = setCells(h, size); assert(cellsOf(h) == size);
}


void test_memory() {
  int cells = 1000;
  LeVM* vm = le_new_vm(cells);
  assert(vm->cells == cells);
  assert(vm->new != vm->old);
  assert(vm->new <= vm->here && vm->here < vm->new + cells);
  le_free_vm(vm);
}


void test_allot_object() {
  LeVM* vm  = le_new_vm(1000);
  Obj  o   = newObj(vm, 3, T_ARRAY);
  Obj  n42 = le_int2obj(42);

  assert(cellsOf(o->header) == 3);
  assert(typeOf(o->header)  == T_ARRAY);

  o->Array.data[0] = n42;
  o->Array.data[1] = n42;
  o->Array.data[2] = n42;
  assert(o->Array.data[0] == n42);
  assert(o->Array.data[1] == n42);
  assert(o->Array.data[2] == n42);

  le_free_vm(vm);
}


void test_bytes_object() {
  LeVM* vm = le_new_vm(1000);
  Obj b = newBytesObj(vm, 3, T_BYTES);

  assert(cellsOf(b->header) == 2);
  assert(typeOf(b->header)  == T_BYTES);
  assert(b->Bytes.size == 3);

  // CellArrayではなくByteArrayとして書き込めているか
  /* byte array? */ assert(&(b->Bytes.data[1]) - &(b->Bytes.data[0]) == 1);
  b->Bytes.data[0] = 42;
  b->Bytes.data[1] = 42;
  b->Bytes.data[2] = 42;
  assert(b->Bytes.data[0] == 42);
  assert(b->Bytes.data[1] == 42);
  assert(b->Bytes.data[2] == 42);
  /* padded & zero cleared? */ assert(b->Bytes.data[3] == 0);

  le_free_vm(vm);
}


void test_temporary_stack() {
  LeVM* vm = le_new_vm(1000);

  Obj n42 = le_int2obj(42);
  Obj* stack = vm->tmp; // スタックのアドレスを書き換えてしまうバグ検出用

  Obj o = newObj(vm, 1, T_ARRAY);
  o->Array.data[0] = n42;
  Push(o);

  Obj p = Pop();
  assert(p == o);
  assert(p->Array.data[0] == n42);

  assert(vm->tmp == stack);

  // 目視でアンダーフローエラー確認
  // Pop(mem);

  le_free_vm(vm);
}

void test_gc_swap() {
  LeVM* vm = le_new_vm(1000);
  Obj* new = vm->new;
  Obj* old = vm->old;

  memSwap(vm);

  assert(new != vm->new);
  assert(old != vm->old);

  le_free_vm(vm);
}

void test_gc_move() {
  LeVM* vm = le_new_vm(1000);
  Obj n42 = le_int2obj(42);

  Obj o = newObj(vm, 1, T_ARRAY);
  o->Array.data[0] = n42;

  memSwap(vm);
  assert(vm->here == vm->new);

  // not obj
  assert(memMove(vm, nil) == nil);
  assert(memMove(vm, n42) == n42);

  // obj
  Obj o2 = memMove(vm, o);
  assert(o2 == (Obj)vm->new); // first of new area
  assert(vm->here != vm->new);  // here-pointer has been moved
  assert(memforwarded(o) == o2); 

  le_free_vm(vm);
}

void test_gc_integration() {
  Obj n42 = le_int2obj(42);
  Obj n43 = le_int2obj(43);

  // check scanning root
  { 
    LeVM* vm = le_new_vm(1000);
    Obj *r = vm->root->Array.data;
    vm->root->Array.data[0] = n42;
    Obj obj = newObj(vm, 1, T_ARRAY);
    vm->root->Array.data[1] = obj;
    obj->Array.data[0] = n43;
    Obj root = vm->root;

    // before
    le_gc(vm);
    // after

    // root
    assert(vm->root != root);
    assert(root->header == (Cell)vm->root);
    assert(vm->root->Array.data[0] == n42);
    root = vm->root;

    // child
    assert(obj != root->Array.data[1]);
    assert(obj->header == (Cell)root->Array.data[1]);
    obj = root->Array.data[1];
    assert(obj->Array.data[0] == n43);

    le_free_vm(vm);
  }

  // check cyclic test
  {
    LeVM* vm = le_new_vm(1000);
    Obj root = vm->root;
    Obj obj1 = newObj(vm, 1, T_ARRAY);
    Obj obj2 = newObj(vm, 1, T_ARRAY);

    // cyclic references
    vm->root->Array.data[0] = obj1;
    obj1->Array.data[0] = obj2;
    obj2->Array.data[0] = vm->root;

    le_gc(vm);

    // root scanned?
    assert(vm->root->Array.data[0] != obj1);
    assert(obj1->header == (Cell)vm->root->Array.data[0]);
    obj1 = vm->root->Array.data[0];

    // obj1 scanned?
    assert(obj1->Array.data[0] != obj2);
    assert(obj2->header == (Cell)obj1->Array.data[0]);
    obj2 = obj1->Array.data[0];

    // obj2 scanned?
    assert(obj2->Array.data[0] != root);
    assert(root->header == (Cell)obj2->Array.data[0]);

    le_free_vm(vm);
  }

  // temporary stack
  {
    LeVM* vm = le_new_vm(1000);
    Obj obj1 = newObj(vm, 1, T_ARRAY);
    Obj obj2 = newObj(vm, 1, T_ARRAY);
    Obj obj3 = newObj(vm, 1, T_ARRAY);
    Cell obj3_header = obj3->header;

    Push(obj1);
    obj1->Array.data[0] = obj2;
    obj2->Array.data[0] = n42;
    // obj3 is going to be deleted

    le_gc(vm);

    Obj moved = Pop();
    assert(obj1->header == (Cell)moved);
    assert(obj2->header == (Cell)moved->Array.data[0]);
    obj2 = moved->Array.data[0];
    assert(obj2->Array.data[0] == n42);
    assert(obj3_header == obj3_header); // not moved

    le_free_vm(vm);
  }

  // byte object
  {
    LeVM* vm = le_new_vm(1000);
    LeObj* b = newBytesObj(vm, 2, T_BYTES);
    b->Bytes.data[0] = 42;
    b->Bytes.data[1] = 43;
    vm->root->Array.data[0] = b;

    le_gc(vm);

    assert(b->header == (Cell)vm->root->Array.data[0]);
    b = (vm->root->Array.data[0]);
    assert(b->Bytes.data[0] == 42);
    assert(b->Bytes.data[1] == 43);

    le_free_vm(vm);
  }
}

// Macros for VM test
// =====================================
#define AssertOK (assert(code == Le_OK))

void test_read_sxp(LeVM* vm) {
  int code;
  Obj x;

  // number
  code = le_read_str(vm, "123");
  assert(le_obj2int(vm->result) == 123);

  code = le_read_str(vm, " -123");
  assert(le_obj2int(vm->result) == -123);


  // symbol
  code = le_read_str(vm, "abc");
  x = vm->result;
  assert(typeOf(x->header) == T_SYMBOL);
  assert(strcmp("abc", symNameStr(x)) == 0);


  // nil
  code = le_read_str(vm, "()");
  x = vm->result;
  assert(x == nil);
  code = le_read_str(vm, " ( ) ");
  x = vm->result;
  assert(x == nil);
  code = le_read_str(vm, "( \n )");
  assert(vm->result == nil);


  // list
  code = le_read_str(vm, "( a b c )");
  x = vm->result;
  char* s = toStr(x);
  assert(strcmp(s, "(a b c)") == 0);
  free(s);

  // skip comment
  code = le_read_str(vm, "; foo\nbar");
  x = vm->result;
  s = toStr(x);
  assert(strcmp(s, "bar") == 0);
  free(s);
}

void test_hash(LeVM* vm) {
  // nil
  assert(le_get_hash(nil) == le_int2obj(0));

  // number
  Obj n42 = le_int2obj(42);
  assert(le_get_hash(n42) == n42);

  // normal object
  Obj t = Sym(True);
  Obj hash = le_get_hash(t);
  le_gc(vm); // change address
  assert(t != Sym(True)); // different address
  assert(le_get_hash(Sym(True)) == hash); // same hash

  // string
  Obj s1 = le_new_str_from(vm, "foo");
  Obj h1 = le_get_hash(s1);
  Obj s2 = le_new_str_from(vm, "foo");
  Obj h2 = le_get_hash(s2);
  assert(h1 == h2);
}

void test_eval_num(LeVM* vm) {
  int code = le_eval_str(vm, "123");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);
}

void test_eval_str(LeVM* vm) {
  int code = le_eval_str(vm, "\"abc\"");
  Obj x = vm->result;
  char* s = toStr(x);
  AssertOK;
  assert(strcmp(s, "\"abc\"") == 0);
  free(s);
  s = le_cstr_of(x);
  assert(strcmp(s, "abc") == 0);
}

void test_eval_let(LeVM* vm) {
  int code = le_eval_str(vm, "(let ((a 123)) a)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);

  // unbound symbol
  code = le_eval_str(vm, "(let ((a 123)) b)");
  x = vm->err;
  char* s = toStr(x);
  assert(strcmp(s, "(error undefined-symbol b)") == 0);
  free(s);
  
  // as do form
  code = le_eval_str(vm, "(let () 123 234)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 234);

  // let*
  code = le_eval_str(vm, "(let ((x 123) (y x)) y)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);
}

void test_eval_fn(LeVM* vm) {
  int code = le_eval_str(vm, "((fn (x) x) 123)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);

  // multiple arguments
  code = le_eval_str(vm, "((fn (a b c) c) 1 2 3)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 3);

  // rest arguments
  code = le_eval_str(vm, "((fn (x . xs) xs) 1 2 3)");
  x = vm->result;
  AssertOK;
  char* s = toStr(x);
  assert(strcmp(s, "(2 3)") == 0);
  free(s);
}

void test_eval_def(LeVM* vm) {
  int code = le_eval_str(vm, "(def x 123)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);

  // same env
  code = le_eval_str(vm, "x");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);
}

void test_eval_if(LeVM* vm) {
  int code = le_eval_str(vm, "(if () 123 234)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 234);

  code = le_eval_str(vm, "(if 1 123 234)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);  
}

void test_eval_arithmetics(LeVM* vm) {
  int code = le_eval_str(vm, "(let ((a 1) (b 2)) (%prim:add a b))");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 3);

  code = le_eval_str(vm, "(%prim:sub 3 2)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 1);

  code = le_eval_str(vm, "(%prim:mul 3 4)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 12);

  code = le_eval_str(vm, "(%prim:div 10 3)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 3);

  code = le_eval_str(vm, "(%prim:div 10 0)");
  x = vm->result;
  assert(code == Le_ERR);
  assert(Second(vm->err) == vm->symTable[SymZeroDivision]);

  code = le_eval_str(vm, "(%prim:mod 10 3)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 1);

  code = le_eval_str(vm, "(%prim:mod 10 0)");
  x = vm->result;
  assert(code == Le_ERR);
  assert(Second(vm->err) == vm->symTable[SymZeroDivision]);  
}

void test_eval_compare(LeVM* vm) {
  int code = le_eval_str(vm, "(let ((a 1) (b 1)) (%prim:eq a b))");
  Obj x = vm->result;
  AssertOK;
  assert(x == Sym(True));

  code = le_eval_str(vm, "(%prim:eq 1 2)");
  x = vm->result;
  AssertOK;
  assert(x == nil);

  code = le_eval_str(vm, "(%prim:not ())");
  x = vm->result;
  AssertOK;
  assert(x == Sym(True));

  code = le_eval_str(vm, "(let ((a 1)) (%prim:not a))");
  x = vm->result;
  AssertOK;
  assert(x == nil);

  code = le_eval_str(vm, "(let ((a 2) (b 1)) (%prim:gt a b))");
  x = vm->result;
  AssertOK;
  assert(x == Sym(True));

  code = le_eval_str(vm, "(%prim:gt 1 1)");
  x = vm->result;
  AssertOK;
  assert(x == nil);

  code = le_eval_str(vm, "(%prim:gt 1 2)");
  x = vm->result;
  AssertOK;
  assert(x == nil);
}

void test_eval_set(LeVM* vm) {
  // local
  int code = le_eval_str(vm, "(let ((a 234)) (set! a 123) a)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);

  // local set returns its value asis
  code = le_eval_str(vm, "(let ((a 234)) (set! a 123))");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);

  // global
  code = le_eval_str(vm, "(def foo 123)");
  x = vm->result;
  AssertOK;
 
  code = le_eval_str(vm, "(set! foo 234)");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 234);

  code = le_eval_str(vm, "foo");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 234);  
}

void test_eval_while(LeVM* vm) {
  int code = le_eval_str(vm, "(let ((i 0)) (while (%prim:gt 10 i) (set! i (%prim:add i 1))) i)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 10);
}

void test_eval_predefined_symbols(LeVM* vm) {
  int code = le_eval_str(vm, "nil");
  Obj x = vm->result;
  AssertOK;
  assert(x == nil);

  code = le_eval_str(vm, "true");
  x = vm->result;
  AssertOK;
  assert(x == Sym(True));
}

void test_eval_quote(LeVM* vm) {
  int code = le_eval_str(vm, "'(123 foo)");
  Obj x = vm->result;
  AssertOK;
  assert(le_obj2int(Car(x)) == 123);
  assert(le_is_symbol(Second(x)));
}

void test_eval_pre_eval(LeVM* vm) {
  int code = le_load_file(vm, TEST_CORELIB_FILE);
  AssertOK;
  
  code = le_eval_str(vm, "(set! %pre-eval (fn (_) 123))"); // all expr returns 123!
  Obj x = vm->result;
  AssertOK;

  code = le_eval_str(vm, "234");
  x = vm->result;
  AssertOK;
  assert(le_obj2int(x) == 123);
}

// Primitives
// =============================================================================
void test_prim_array(LeVM* vm) {
  char* src =
    "(let ((Xs (%prim:array-new 42)))"
    "  (%prim:array-set! Xs 41 123)"
    "  (%prim:add (%prim:array-get Xs 41)"
    "             (%prim:array-len Xs)))"
    ;
  int code = le_eval_str(vm, src);
  AssertOK;
  assert(le_obj2int(vm->result) == 123 + 42);

  // Deny negative index
  code = le_eval_str(vm, "(%prim:array-get (%prim:array-new 42) -1)");
  assert(code == Le_ERR);
  code = le_eval_str(vm, "(%prim:array-set! (%prim:array-new 42) -1 nil)");
  assert(code == Le_ERR);

  // Deny out of range
  code = le_eval_str(vm, "(%prim:array-get (%prim:array-new 42) 42)");
  assert(code == Le_ERR);
  code = le_eval_str(vm, "(%prim:array-set! (%prim:array-new 42) 42 nil)");
  assert(code == Le_ERR);
}

void test_prim_pair(LeVM* vm) {
  char* src =
    "(let ((Xs (%prim:cons 1 2)))"
    "  (%prim:set-car! Xs 123)"
    "  (%prim:set-cdr! Xs 42)"
    "  (%prim:add (%prim:car Xs)"
    "             (%prim:cdr Xs)))"
    ;
  int code = le_eval_str(vm, src);
  AssertOK;
  assert(le_obj2int(vm->result) == 123 + 42);
}

void test_prim_str(LeVM* vm) {
  char* src;
  int code;
    
  src = "(%prim:str-eq \"foo\" \"foo\")";
  code = le_eval_str(vm, src);
  AssertOK;
  assert(vm->result == Sym(True));

  src = "(%prim:str-eq \"foo\" \"bar\")";
  code = le_eval_str(vm, src);
  AssertOK;
  assert(vm->result == nil);
}

#define test(Name) { printf("%-30s ", #Name); test_##Name(); printf("ok\n"); }
#define testVM(Name) {                          \
    printf("%-30s ", #Name);                    \
    LeVM* vm = le_create_vm();                  \
    test_##Name(vm);                            \
    le_free_vm(vm);                             \
    printf("ok\n");                             \
  }

void test_all() {
  printf("run test...\n");
  test(align);
  test(tagged_pointer);
  test(object_header);
  test(memory);
  test(allot_object);
  test(bytes_object);
  test(temporary_stack);
  test(gc_swap);
  test(gc_move);
  test(gc_integration);
  testVM(read_sxp);
  testVM(hash);
  // eval
  testVM(eval_num);
  testVM(eval_str);
  testVM(eval_let);
  testVM(eval_fn);
  testVM(eval_def);
  testVM(eval_if);
  testVM(eval_arithmetics);
  testVM(eval_compare);
  testVM(eval_set);
  testVM(eval_while);
  testVM(eval_predefined_symbols);
  testVM(eval_quote);
  testVM(eval_pre_eval);
  // primitives
  testVM(prim_array);
  testVM(prim_pair);
  testVM(prim_str);
}


int main(void) {
  test_all();
  return 0;
}
  
