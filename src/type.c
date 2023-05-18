#include "mycc.h"

/* an empty int variable with sizeof 8 */
Type *ty_int = &(Type){TY_INT, 8};

/* judge wheather the variable is an integer */
bool is_integer(Type *ty) {
  return ty->kind == TY_INT;
}

/* copy a type */
Type* copy_type(Type* ty) {
  Type* ret = calloc(1, sizeof(Type));
  *ret = *ty;
  return ret;
}

/* pointer type to the base */
Type *pointer_to(Type *base) {
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_PTR;
  ty->size = 8;
  ty->base = base;
  return ty;
}

/* function type, get return */
Type* func_type(Type* return_ty) {
  Type* ty = calloc(1, sizeof(Type));
  ty->kind = TY_FUNC;
  ty->return_ty = return_ty;
  return ty;
}

/* array type, create the type */
Type* array_of(Type* base, int len) {
  Type* ty = calloc(1, sizeof(Type));
  ty->kind = TY_ARRAY;
  /* array size is the sum of all the elements */
  ty->size = base->size * len;
  ty->base = base;
  ty->array_len = len;
  return ty;
}

/* add type for all the nodes */
void add_type(Node *node) {
  /* judge wheather the node is empty or its type has value, if true then return */
  if (!node || node->ty)
    return;

  /* recursively visit all the nodes */
  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  /* visit the lists of body to add types */
  for (Node *n = node->body; n; n = n->next)
    add_type(n);
  /* visit the lists of args to add types */
  for (Node *n = node->args; n; n = n->next)
    add_type(n);

  switch (node->kind) {
    /* set same to the left */
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_NEG:
      node->ty = node->lhs->ty;
      return;
    /* set same to the left, but left cannot be array */
    case ND_ASSIGN:
      if (node->lhs->ty->kind == TY_ARRAY)
        error_tok(node->lhs->tk, "not an lvalue");
      node->ty = node->lhs->ty;
      return;
    /* set as `int` */
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_NUM:
    case ND_FUNCALL:
      node->ty = ty_int;
      return;
    /* set as `variable` */
    case ND_VAR:
      node->ty = node->var->ty;
      return;
    /* set as pointer to left */
    case ND_ADDR:
      /* if array, then point to base */
      if (node->lhs->ty->kind == TY_ARRAY)
        node->ty = pointer_to(node->lhs->ty->base);
      else
        node->ty = pointer_to(node->lhs->ty);
      return;
    /* set as address */
    case ND_DEREF:
      /* if no base, then error */
      if (!node->lhs->ty->base)
        error_tok(node->tk, "invalid pointer dereference");
      node->ty = node->lhs->ty->base;
      return;
  }
}
