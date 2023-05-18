#include "mycc.h"

int main(int argc, char**argv)
{
  /* if argc != 2, show errors: invalid arguments */
  if (argc != 2)
    error("%s: invalid number of arguments", argv[0]);
  
  /* read file `argv[1]` */
  char* content = read_file(argv[1]);
  
  /* tokenize and parse */
  Token* tk = tokenize(argv[1], content);
  Function* prog = parse(tk);

  /* tranverse the AST to emit assembly */
  codegen(prog);

  return 0;     // right return
}
