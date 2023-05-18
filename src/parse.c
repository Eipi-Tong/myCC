#include "mycc.h"

/* all local variable instances created during parsing are accumulated to this list */
Obj* locals;

// /* an empty int variable with sizeof 8 */
// extern Type* ty_int;

static Type *declspec(Token **rest, Token *tk);
static Type *declarator(Token **rest, Token *tk, Type *ty);
static Node* declaration(Token** rest, Token* tk);
static Node* compound_stmt(Token** rest, Token* tk);
static Node* stmt(Token** rest, Token* tk);
static Node* expr_stmt(Token** rest, Token* tk);
static Node* expr(Token** rest, Token* tk);
static Node* assign(Token**rest, Token* tk);
static Node* equality(Token** rest, Token* tk);
static Node* relational(Token** rest, Token* tk);
static Node* add(Token** rest, Token* tk);
static Node* mul(Token** rest, Token* tk);
static Node* postfix(Token** rest, Token* tk);
static Node* unary(Token** rest, Token* tk);
static Node* primary(Token** rest, Token* tk);

// find a local variable by name
static Obj* find_var(Token* tk) {
  for (Obj* var = locals; var; var = var->next) {
    if (strlen(var->name) == tk->len && !strncmp(tk->loc, var->name, tk->len)) {
      return var;
    }
  }
  return NULL;
}

// new node
static Node* new_node(NodeKind kind, Token* tk) {
  Node* node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->tk = tk;
  return node;
}

// new binary node
static Node* new_binary(NodeKind kind, Node* lhs, Node* rhs, Token* tk) {
  Node* node = new_node(kind, tk);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

// new unary node
static Node* new_unary(NodeKind kind, Node* expr, Token* tk) {
  Node* node = new_node(kind, tk);
  node->lhs = expr;    // the exprssion is on the left
  return node;
}

// new numerical node
static Node* new_num(int val, Token* tk) {
  Node* node = new_node(ND_NUM, tk);
  node->val = val;
  return node;
}

// new variable 
static Node* new_var_node(Obj* var, Token* tk) {
  Node* node = new_node(ND_VAR, tk);
  node->var = var;
  return node;
}

// create new list of variables
static Obj* new_lvar(char* name, Type* ty) {
  Obj* var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  // insert variable into the heading
  var->next = locals;
  locals = var;
  return var;
}

// get ident from token
static char *get_ident(Token *tk) {
  if (tk->kind != TK_IDENT)
    error_tok(tk, "expected an identifier");
  return strndup(tk->loc, tk->len);
}

// get number from token
static int get_number(Token* tk) {
  if (tk->kind != TK_NUM)
    error_tok(tk, "expected a number");
  return tk->val;
}

// declspec = "int"
// declarator specifier
static Type *declspec(Token **rest, Token *tk) {
  *rest = skip(tk, "int");
  return ty_int;
}

// func-params = (param ("," param)*? ")"
// param       = declspec declarator
static Type* func_params(Token** rest, Token* tk, Type* ty) {
  Type head = {};
  Type* cur = &head;

  while(!equal(tk, ")")) {
    // func-params = (param ("," param)*? ")"
    // param       = declspec declarator
    if (cur != &head)
      tk = skip(tk, ",");
    Type* basety = declspec(&tk ,tk);
    Type* ty = declarator(&tk, tk, basety);
    // copy the type into params
    cur = cur->next = copy_type(ty);
  }

  ty = func_type(ty);
  ty->params = head.next;
  *rest = tk->next;
  return ty;
}

// type-suffix = "(" func-params
//             | "[" num "]" type-suffix
//             | e
static Type* type_suffix(Token** rest, Token* tk, Type* ty) {
  if (equal(tk, "("))
    return func_params(rest, tk->next, ty);
  
  if (equal(tk, "[")) {
    int sz = get_number(tk->next);
    tk = skip(tk->next->next, "]");
    ty = type_suffix(rest, tk, ty);
    return array_of(ty, sz);
  }
  *rest = tk;
  return ty;
}

// declarator = "*"* ident type-suffix
static Type *declarator(Token **rest, Token *tk, Type *ty) {
  while (consume(&tk, tk, "*"))
    ty = pointer_to(ty);

  if (tk->kind != TK_IDENT)
    error_tok(tk, "expected a variable name");

  ty = type_suffix(rest, tk->next, ty);
  // ident
  ty->name = tk;
  return ty;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tk) {
  // declspec
  Type *basety = declspec(&tk, tk);

  Node head = {};
  Node *cur = &head;
  int i = 0;
  
  // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
  while (!equal(tk, ";")) {
    if (i++ > 0)
      tk = skip(tk, ",");

    // declarator
    Type *ty = declarator(&tk, tk, basety);
    Obj *var = new_lvar(get_ident(ty->name), ty);

    // has been in locals
    if (!equal(tk, "="))
      continue;
    
    // parse token behind "="
    Node *lhs = new_var_node(var, ty->name);
    // parse assign statement
    Node *rhs = assign(&tk, tk->next);
    Node *node = new_binary(ND_ASSIGN, lhs, rhs, tk);
    // put into the statement
    cur = cur->next = new_unary(ND_EXPR_STMT, node, tk);
  }

  Node *node = new_node(ND_BLOCK, tk);
  node->body = head.next;
  *rest = tk->next;
  return node;
}

// stmt = "return" expr ";" 
//       | if "(" expr ")" stmt ("else" stmt)? 
//       | for "(" expr_stmt expr? ";" expr? ")" stmt
//       | while "(" expr ")" stmt
//       | "{" compound_stmt
//       | expr_stmt
static Node* stmt(Token** rest, Token* tk) {
  // "return" expr ";" 
  if (equal(tk, "return")) {
    Node *node = new_node(ND_RETURN, tk);
    node->lhs = expr(&tk, tk->next);
    *rest = skip(tk, ";");
    return node;
  }
  // if "(" expr ")" stmt ("else" stmt)? 
  if (equal(tk, "if")) {
    Node *node = new_node(ND_IF, tk);
    tk = skip(tk->next, "(");
    node->cond = expr(&tk, tk);
    tk = skip(tk, ")");
    node->then = stmt(&tk, tk);
    if (equal(tk, "else")) {
      node->els = stmt(&tk, tk->next);
    }
    *rest = tk;
    return node;
  }
  // for "(" expr_stmt expr? ";" expr? ")" stmt
  if (equal(tk, "for")) {
    Node *node = new_node(ND_FOR, tk);
    tk = skip(tk->next, "(");
    node->init = expr_stmt(&tk, tk);
    if (!equal(tk, ";")) {
      node->cond = expr(&tk, tk);
    }
    tk = skip(tk, ";");
    if (!equal(tk, ")")) {
      node->inc = expr(&tk, tk);
    }
    tk = skip(tk, ")");
    node->then = stmt(rest, tk);
    return node;
  }
  // while "(" expr ")" stmt
  if (equal(tk, "while")) {
    Node *node = new_node(ND_FOR, tk);
    tk = skip(tk->next, "(");
    node->cond = expr(&tk, tk);
    tk = skip(tk, ")");
    node->then = stmt(rest, tk);
    return node;
  }
  // "{" compound_stmt
  if (equal(tk, "{")) {
    return compound_stmt(rest, tk->next);
  }
  // expr_stmt
  return expr_stmt(rest, tk);
}

// compound_stmt = (declaration | stmt)* "}"
static Node* compound_stmt(Token** rest, Token* tk) {
  Node *node = new_node(ND_BLOCK, tk);
  Node head = {};
  Node *cur = &head;
  // (declaration | stmt)* "}"
  while (!equal(tk, "}")) {
    if (equal(tk, "int"))
      cur = cur->next = declaration(&tk, tk);
    else
      cur = cur->next = stmt(&tk, tk);
    // add type for the node
    add_type(cur);
  }
  // set node->body the statement
  node->body = head.next;
  *rest = tk->next;
  return node;
}

// expr_stmt = expr? ";"
static Node* expr_stmt(Token** rest, Token* tk) {
  // ";"
  if (equal(tk, ";")) {
    *rest = tk->next;
    return new_node(ND_BLOCK, tk);
  }
  // expr ";"
  Node *node = new_node(ND_EXPR_STMT, tk);
  node->lhs = expr(&tk, tk);
  *rest = skip(tk, ";");
  return node;
}

// expr = assign
static Node* expr(Token** rest, Token* tk) {
  return assign(rest, tk);
}

// assign = equality ("=" assign)?
static Node* assign(Token**rest, Token* tk) {
  Node* node = equality(&tk, tk);
  // ("=" assign)? recursive assign 
  if (equal(tk, "=")) {
    node = new_binary(ND_ASSIGN, node, assign(&tk, tk->next), tk);
  }
  *rest = tk;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node* equality(Token** rest, Token* tk) {
  Node* node = relational(&tk, tk);
  // ("==" relational | "!=" relational)*
  while (1) {
    Token* start = tk;
    // "==" relational
    if (equal(tk, "==")) {
      node = new_binary(ND_EQ, node, relational(&tk, tk->next), start);
      continue;
    }
    // "!=" relational
    if (equal(tk, "!=")) {
      node = new_binary(ND_NE, node, relational(&tk, tk->next), start);
      continue;
    }
    *rest = tk;
    return node;
  }
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
static Node* relational(Token** rest, Token* tk) {
  Node* node = add(&tk, tk);
  // ("<" add | "<=" add | ">" add | ">=" add)*
  while (1) {
    Token* start = tk;
    // "<" add
    if (equal(tk, "<")) {
      node = new_binary(ND_LT, node, add(&tk, tk->next), start);
      continue;
    }
    // "<=" add
    if (equal(tk, "<=")) {
      node = new_binary(ND_LE, node, add(&tk, tk->next), start);
      continue;
    }
    // ">" add, same as "<"
    if (equal(tk, ">")) {
      node = new_binary(ND_LT, add(&tk, tk->next), node, start);
      continue;
    }
    // ">=" add, same as "<="
    if (equal(tk, ">=")) {
      node = new_binary(ND_LE, add(&tk, tk->next), node, start);
      continue;
    }
    *rest = tk;
    return node;
  }
}

// Like `+`, `-` is overloaded for the pointer type.
static Node* new_add(Node* lhs, Node* rhs, Token* tk) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (is_integer(lhs->ty) && is_integer(rhs->ty))
    return new_binary(ND_ADD, lhs, rhs, tk);
  
  if (lhs->ty->base && rhs->ty->base)
    error_tok(tk, "invalid operands");
  
  // Canoicalize `num + ptr` to `ptr + num`.
  if (!lhs->ty->base && rhs->ty->base) {
    Node* tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // ptr + num ( * size)
  rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tk), tk);
  return new_binary(ND_ADD, lhs, rhs, tk);
}

// Like `+`, `-` is overloaded for the pointer type.
static Node* new_sub(Node* lhs, Node* rhs, Token* tk) {
  // add type for left and right
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_integer(lhs->ty) && is_integer(rhs->ty))
    return new_binary(ND_SUB, lhs, rhs, tk);
  
  // ptr - num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tk), tk);
    add_type(rhs);
    Node* node = new_binary(ND_SUB, lhs, rhs, tk);
    node->ty = lhs->ty;
    return node;
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    Node* node = new_binary(ND_SUB, lhs, rhs, tk);
    node->ty = ty_int;
    return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tk), tk);
  }

  error_tok(tk, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node* add(Token** rest, Token* tk) {
  Node* node = mul(&tk, tk);
  // ("+" mul | "-" mul)*
  while (1) {
    Token* start = tk;
    // "+" mul
    if (equal(tk, "+")) {
      node = new_add(node, mul(&tk, tk->next), start);
      continue;
    }
    // "-" mul
    if (equal(tk, "-")) {
      node = new_sub(node, mul(&tk, tk->next), start);
      continue;
    }
    *rest = tk;
    return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
static Node* mul(Token** rest, Token* tk) {
  Node* node = unary(&tk, tk);
  // ("*" unary | "/" unary)*
  while (1) {
    Token* start = tk;
    // "*" unary
    if (equal(tk, "*")) {
      node = new_binary(ND_MUL, node, unary(&tk, tk->next), start);
      continue;
    }
    // "/" unary
    if (equal(tk, "/")) {
      node = new_binary(ND_DIV, node, unary(&tk, tk->next), start);
      continue;
    }
    *rest = tk;
    return node;
  }
}

// unary = ("+" | "-" | "&" | "*") unary | postfix
static Node* unary(Token** rest, Token* tk) {
  // "+" unary
  if (equal(tk, "+"))
    return unary(rest, tk->next);
  // "-" unary
  if (equal(tk, "-"))
    return new_unary(ND_NEG, unary(rest, tk->next), tk);
  // "&" unary
  if (equal(tk, "&"))
    return new_unary(ND_ADDR, unary(rest, tk->next), tk);
  // "*" unary
  if (equal(tk, "*"))
    return new_unary(ND_DEREF, unary(rest, tk->next), tk);

  // postfix
  return postfix(rest, tk);
}

// postfix = primary ("[" expr "]")*
static Node* postfix(Token** rest, Token* tk) {
  Node* node = primary(&tk, tk);
  // ("[" expr "]")*
  while (equal(tk, "[")) {
    // x[y] same as *(x+y)
    Token* start = tk;
    Node* idx = expr(&tk, tk->next);
    tk = skip(tk, "]");
    node = new_unary(ND_DEREF, new_add(node, idx, start), start);
  }
  *rest = tk;
  return node;
}

// funcall = ident "(" (assign ("," assign)*)? ")"
static Node* funcall(Token** rest, Token* tk) {
  Token* start = tk;
  tk = tk->next->next;
  Node head = {};
  Node* cur = &head;
  while (!equal(tk, ")")) {
    if (cur != &head)
      tk = skip(tk, ",");
    // assign
    cur = cur->next = assign(&tk, tk);
  }
  *rest = skip(tk, ")");
  Node* node = new_node(ND_FUNCALL, start);
  // ident
  node->funcname = strndup(start->loc, start->len);
  node->args = head.next;
  return node;
}

// primary = "(" expr ")" | "sizeof" unary | ident func-arg? | num
static Node* primary(Token** rest, Token* tk) {
  // "(" expr ")"
  if (equal(tk, "(")) {
    Node* node = expr(&tk, tk->next);
    *rest = skip(tk, ")");
    return node;
  }
  // "sizeof" unary
  if (equal(tk, "sizeof")) {
    Node* node = unary(rest, tk->next);
    add_type(node);
    return new_num(node->ty->size, tk);
  }
  // ident func-arg? 
  if (tk->kind == TK_IDENT) {
    // function call
    if (equal(tk->next, "("))
      return funcall(rest, tk);
    
    // find the variable node's location
    Obj* var = find_var(tk);
    if (!var) {
      error_tok(tk, "undefined variable");
    }
    *rest = tk->next;
    return new_var_node(var, tk);
  }
  // num
  if (tk->kind == TK_NUM) {
    Node* node = new_num(tk->val, tk);
    *rest = tk->next;
    return node;
  }
  error_tok(tk, "expected an expression");
}

// put params into locals
static void create_param_lvars(Type* param) {
  if (param) {
    // recurisively find locals
    create_param_lvars(param->next);
    // get param's name and put it into locals
    new_lvar(get_ident(param->name), param);
  }
}

// function-definition = declspec declarator "{" compound_stmt*
static Function* function(Token** rest, Token* tk) {
  // declspec
  Type* ty = declspec(&tk, tk);
  // declarator? ident "(" ")"
  ty = declarator(&tk, tk, ty);
  // clear the global variable `locals`
  locals = NULL;

  // new a function
  Function* fn = calloc(1, sizeof(Function));
  fn->name = get_ident(ty->name);
  // get function params
  create_param_lvars(ty->params);
  fn->params = locals;

  tk = skip(tk, "{");
  // set function body as statement
  fn->body = compound_stmt(rest, tk);
  fn->locals = locals;
  return fn;
}

// parse entrance function
// program = function-definition*
Function* parse(Token* tk) {
  Function head = {};
  Function* cur = &head;
  while(tk->kind != TK_EOF)
    cur = cur->next = function(&tk, tk);
  return head.next;
}
