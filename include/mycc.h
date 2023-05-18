#define _POSIX_C_SOURCE 200809L   // for function strndup()
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>    // for variable parameters
#include <stdbool.h>   // for structure `bool`
#include <string.h>    // for function memcmp()
#include <ctype.h>     // for check number and punctuator
#include <errno.h>

typedef struct Type Type;
typedef struct Node Node;

/*
 * tokenize.c
*/
// token kind
typedef enum {
  TK_IDENT,   // identifiers
  TK_PUNCT,   // punctuators
  TK_KEYWORD, // keywords
  TK_NUM,     // numeric literals
  TK_EOF      // end-of-file markers
} TokenKind;

// token
typedef struct Token Token;
struct Token {
  TokenKind kind;  // token type
  int val;         // if TK_NUM, then its value
  int len;         // token length
  char* loc;       // token location
  Token* next;     // next token
};

/* judge whether the current token is `s` */
bool equal(Token* tk, char* s);
/* skip the current token `s` */
Token* skip(Token* tk, char* s);
/* consume str, if success return true, otherwise return false */
bool consume(Token** rest, Token* tk, char* str);
/* tokenize a given string and return new tokens */
Token* tokenize(char* filename, char* p);
/* debug and show all the tokens */
void showTokens(Token* tk);


/*
 * parse.c
*/
/* local variable */
typedef struct Obj Obj;
struct Obj {
  Obj* next;     // next variable
  char* name;    // variable name
  Type* ty;      // type
  int offset;    // offset from rbp
};

/* function */
typedef struct Function Function;
struct Function {
  Function* next;  // next function
  char* name;      // function name
  Obj* params;     // function params 

  Node* body;      // function body
  Obj* locals;     // local variables
  int stack_size;  // stack size
};

/* AST Node Type */
typedef enum {
  ND_ADD,        // +
  ND_SUB,        // -
  ND_MUL,        // *
  ND_DIV,        // /
  ND_NEG,        // unary -
  ND_EQ,         // ==
  ND_NE,         // !=
  ND_LT,         // <
  ND_LE,         // <=
  ND_ASSIGN,     // =
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_BLOCK,      // { ... }
  ND_FUNCALL,    // function call
  ND_EXPR_STMT,  // expression statement
  ND_VAR,        // variable
  ND_NUM         // integer
} NodeKind;

/* AST Node  */
struct Node {
  NodeKind kind;  // node kind
  Node* next;     // next node
  Type* ty;       // type, e.g. int or pointer to int
  Token* tk;      // representative token

  Node* lhs;      // left-handed side
  Node* rhs;      // right-handed side

  // "if" or "for" statement
  Node* cond;     // if's condition
  Node* then;     // if's todo
  Node* els;      // else's todo
  Node* init;     // for's initial state
  Node* inc;      // for's next 

  Node* body;     // block
  // function call
  char* funcname; // function name
  Node* args;     // function args
  Obj* var;       // used if kind == ND_VAR
  int val;        // used if kind == ND_NUM
};

/* parse all the tokens and get the functions */
Function* parse(Token* tk);


/*
 * type.c
*/
/* Variable Type Kind, eg. int, ptr, func or array */
typedef enum {
  TY_INT,    // integer
  TY_PTR,    // pointer
  TY_FUNC,   // function
  TY_ARRAY   // array
} TypeKind;

/* Variable Type */
struct Type {
  TypeKind kind;  // Type Kind

  int size;       // sizeof() value

  Type* base;       // pointer to base
  Token* name;      // declaration

  // array type
  int array_len;    // array length

  // function type
  Type* return_ty;  // function return
  Type* params;     // function params
  Type* next;       // function next
};

/* an empty int variable with sizeof 8 */
extern Type* ty_int;

/* judge wheather the variable is an integer */
bool is_integer(Type* ty);
/* copy a type */
Type* copy_type(Type* ty);
/* pointer type to the base */
Type* pointer_to(Type* base);
/* function type, get return */
Type* func_type(Type* return_ty);
/* array type, create the type */
Type* array_of(Type* base, int size);
/* add type for all the nodes */
void add_type(Node* node);


/*
 * codegen.c
*/
/* generate assembly code, according to the programs */
void codegen(Function* prog);


/*
 * utility.c
*/
/* returns the content's of a given file. */
char* read_file(char* path);
/* report an error and exit */
void error(char* fmt, ...);
/* set error location */
void error_at(char* loc, char* fmt, ...);
/* set token's error location */
void error_tok(Token* tk, char* fmt, ...);
