#include "mycc.h"

// stack depth
static int depth;
// registers  
static char* argreg[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
// current function
static Function* current_fn;

static void gen_expr(Node* node);

// count the code segments
static int count(void) {
  static int i = 1;
  return i++;
}

// push into stack
static void push(void) {
  printf("  push %%rax\n");
  depth++;
}

// pull from stack
static void pop(char* arg) {
  printf("  pop %s\n", arg);
  depth--;
}

// round up `n` to the nearest multiple of `align`
// eg. align_to(5, 8) returns 8 and align_to(11, 8) returns 16
static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// compute the absolute address of a given node
// it's a error if the given node does not reside in memory
static void gen_addr(Node* node) {
  switch (node->kind) {
    // variable
    case ND_VAR:
      printf("  lea %d(%%rbp), %%rax\n", node->var->offset);
      return;
    // dereference
    case ND_DEREF:
      gen_expr(node->lhs);
      return;
  }
  error_tok(node->tk, "not an lvalue");
}

// Load a value from where %rax is pointing to.
static void load(Type *ty) {
  if (ty->kind == TY_ARRAY) {
    return;
  }
  printf("  mov (%%rax), %%rax\n");
}

// Store %rax to an address that the stack top is pointing to.
static void store(void) {
  pop("%rdi");   // Here is %rdi
  printf("  mov %%rax, (%%rdi)\n");
}

// generate code for a given node
static void gen_expr(Node* node) {
  // generate root node
  switch(node->kind) {
    // load number from %rax
    case ND_NUM:
      printf("  mov $%d, %%rax\n", node->val);
      return;
    // neg the %rax
    case ND_NEG:
      gen_expr(node->lhs);
      printf("  neg %%rax\n");
      return;
    case ND_VAR:
      gen_addr(node);
      load(node->ty);
      return;
    case ND_DEREF:
      gen_expr(node->lhs);
      load(node->ty);
      return;
    case ND_ADDR:
      gen_addr(node->lhs);
      return;
    case ND_ASSIGN:
      gen_addr(node->lhs);
      push();
      gen_expr(node->rhs);
      store();
      return;
    // function call
    case ND_FUNCALL: {
      int nargs = 0;
      for (Node* arg = node->args; arg; arg = arg->next) {
        gen_expr(arg);
        push();
        nargs++;
      }
      for (int i = nargs - 1; i >= 0; i--)
        pop(argreg[i]);
      printf("  mov $0, %%rax\n");
      printf("  call %s\n", node->funcname);
      return;
    }
  }
  // first right, then left
  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("%rdi");   // Here is %rdi
  switch(node->kind) {
    case ND_ADD:
      printf("  add %%rdi, %%rax\n");
      return;
    case ND_SUB:
      printf("  sub %%rdi, %%rax\n");
      return;
    case ND_MUL:
      printf("  imul %%rdi, %%rax\n");
      return;
    case ND_DIV:
      printf("  cqo\n");
      printf("  idiv %%rdi\n");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      printf("  cmp %%rdi, %%rax\n");
      if (node->kind == ND_EQ)
        printf("  sete %%al\n");
      else if (node->kind == ND_NE)
        printf("  setne %%al\n");
      else if (node->kind == ND_LT)
        printf("  setl %%al\n");
      else if (node->kind == ND_LE)
        printf("  setle %%al\n");
      printf("  movzb %%al, %%rax\n");
      return;
  }
  error_tok(node->tk, "invalid expression");
}

// generate the statements
static void gen_stmt(Node* node) {
  switch (node->kind) {
    case ND_IF: {
      int c = count();
      gen_expr(node->cond);
      printf("  cmp $0, %%rax\n");
      printf("  je  .L.else.%d\n", c);
      gen_stmt(node->then);
      printf("  jmp .L.end.%d\n", c);
      printf(".L.else.%d:\n", c);
      if (node->els)
        gen_stmt(node->els);
      printf(".L.end.%d:\n", c);
      return;
    }
    case ND_FOR: {
      int c = count();
      if (node->init) {
        gen_stmt(node->init);
      }
      printf(".L.begin.%d:\n", c);
      if (node->cond) {
        gen_expr(node->cond);
        printf("  cmp $0, %%rax\n");
        printf("  je .L.end.%d\n", c);
      }
      gen_stmt(node->then);
      if (node->inc) {
        gen_expr(node->inc);
      }
      printf("  jmp .L.begin.%d\n", c);
      printf(".L.end.%d:\n", c);
      return;
    }
    case ND_BLOCK:
      for (Node* n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      return;
    case ND_RETURN:
      gen_expr(node->lhs);
      printf("  jmp .L.return.%s\n", current_fn->name);
      return;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      return;
  }
  error_tok(node->tk, "invalid statement");
}

// assign offsets to local variables
static void assign_lvar_offsets(Function* prog) {
  for (Function* fn = prog; fn; fn = fn->next) {
    int offset = 0;
    for (Obj* var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

/* generate assembly code, according to the programs */
void codegen(Function* prog) {
  // assign offsets to local variables;
  assign_lvar_offsets(prog);
  for (Function* fn = prog; fn; fn = fn->next) {
    // assembly language for x86
    printf("  .globl %s\n", fn->name);
    printf("%s:\n", fn->name);
    current_fn = fn;

    // prologue
    printf("  push %%rbp\n");
    printf("  mov %%rsp, %%rbp\n");
    printf("  sub $%d, %%rsp\n", fn->stack_size);

    // svae passed-by register arguments to the stack
    int i = 0;
    for (Obj* var = fn->params; var; var = var->next) {
      printf("  mov %s, %d(%%rbp)\n", argreg[i++], var->offset);
    }

    // emit code
    gen_stmt(fn->body);
    // judge whether the stack depth is 0
    assert(depth == 0);

    // epilogue
    printf(".L.return.%s:\n", fn->name);
    printf("  mov %%rbp, %%rsp\n");
    printf("  pop %%rbp\n");
    printf("  ret\n");
  }
}
