#include "mycc.h"

/* input filename */
char* cur_filename;

/* input string - global variable */
char* cur_input;

/* returns the content's of a given file. */
char* read_file(char* path) {
  FILE* fp;

  if (strcmp(path, "-") == 0) {
    /* By convention, read from stdin if a given filename is "-". */
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp)
      error("cannot open %s: %s", path, strerror(errno));
  }

  char* buf;
  size_t buflen;
  /* ask for an area of buffer */
  FILE* out = open_memstream(&buf, &buflen);

  /* read the entire file. */
  while (1) {
    char buf2[4096];
    int n = fread(buf2, 1, sizeof(buf2), fp);
    if (n == 0)
      break;
    fwrite(buf2, 1, n, out);
  }

  if (fp != stdin)
    fclose(fp);
  
  /* Make sure that the last line is properly terminated with '\n'. */
  fflush(out);
  if (buflen == 0 || buf[buflen - 1] != '\n')
    fputc('\n', out);
  fputc('\0', out);
  fclose(out);
  return buf;
}


/* report an error and exit */
void error(char* fmt, ...) {
  va_list ap;     /* a macro used for variable parameters */
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);        /* wrong return */
}

/* reports an error message in the following format and exit.
 * foo.c:10: x = y + 1;
 *               ^ <error message here>
*/ 
static void verror_at(char* loc, char* fmt, va_list ap) {
  /* find a line containing `loc`. */
  char *line = loc;
  while (cur_input < line && line[-1] != '\n')
    line--;

  char *end = loc;
  while (*end != '\n')
    end++;

  /* get a line number. */
  int line_no = 1;
  for (char *p = cur_input; p < line; p++)
    if (*p == '\n')
      line_no++;

  /* print out the line. */
  int indent = fprintf(stderr, "%s:%d: ", cur_filename, line_no);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  /* show the error message. */
  int pos = loc - line + indent;

  fprintf(stderr, "%*s", pos, "");  /* print pos spaces */
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

/* set error location */
void error_at(char* loc, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(loc, fmt, ap);
}

/* set error location */
void error_tok(Token* tk, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tk->loc, fmt, ap);
}
