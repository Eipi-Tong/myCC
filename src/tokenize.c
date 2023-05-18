#include "mycc.h"

/* current input filename, defined in `utility.c` */
extern char* cur_filename;

/* current input string - global variable, defined in `utility.c` */
extern char* cur_input;

/* judge whether the current token is `s` */
bool equal(Token* tk, char* s) {
  /* if tk == s, `memcmp` return 0 and s.len == tk.len */
  return memcmp(tk->loc, s, tk->len) == 0 && s[tk->len] == '\0';
}

/* skip the current token `s` */
Token* skip(Token* tk, char* s) {
  /* ensure the current token is `s` */
  if (!equal(tk, s))
    error_tok(tk, "expected '%s'", s);
  return tk->next;
}

/* consume str, if success return true, otherwise return false */
bool consume(Token** rest, Token* tk, char* str) {
  if (equal(tk, str)) {
    /* tk == str, then *rest = tk->next */
    *rest = tk->next;
    return true;
  }
  /* if not success, then keep same */
  *rest = tk;
  return false;
}

/* create a new token */
static Token* new_token(TokenKind kind, char* start, char* end) {
  Token* tk = calloc(1, sizeof(Token));
  tk->kind = kind;
  tk->loc = start;
  tk->len = end - start;
  return tk;
}

/* judge wheather the first several digits of p is similar with q */
static bool startswith(char* p, char* q) {
  return strncmp(p, q, strlen(q)) == 0;
}

/* if c is valid as the first character of an identifier, return true */
static bool is_ident1(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

/* if c is valid as the non-first character of an identifier, return true */
static bool is_ident2(char c) {
  return is_ident1(c) || (c >= '0' && c <= '9');
}

/* read a punctuator token from p and returns its length */
static int read_punct(char* p) {
  if (startswith(p, "==") || startswith(p, "!=")
   || startswith(p, "<=") || startswith(p, ">="))
    return 2;
  return ispunct(*p) ? 1 : 0;
}

/* judge what key word is the token */
static bool is_keyword(Token* tk) {
  static char* kw[] = {"return", "if", "else", "for", "while", "int", "sizeof"};
  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (equal(tk, kw[i]))
      return true;
  }
  return false;
}

/* convert keywords into tokens */
static void convert_keywords(Token* tk) {
  for (Token* t = tk; t->kind != TK_EOF; t= t->next) {
    if (is_keyword(t)) {
      t->kind = TK_KEYWORD;
    }
  }
}

/* tokenize a given string and return new tokens */
Token* tokenize(char* filename, char* p) {
  /* the foreign variable, used for show errors */
  cur_filename = filename;
  cur_input = p;
  /* set a head for the token list */
  Token head = {};
  Token* cur = &head;

  /* pass the whole program */
  while(*p) {
    /* skip line comments */
    if (startswith(p, "//")) {
      p += 2;
      while (*p != '\n')
        p++;
      continue;
    }

    /* skip block comments */
    if (startswith(p, "/*")) {
      char *q = strstr(p + 2, "*/");
      if (!q)
        error_at(p, "unclosed block comment");
      p = q + 2;
      continue;
    }

    /* skip white space characters */
    if (isspace(*p)) {
      p++;
      continue;
    }

    /* numeric literal */
    if (isdigit(*p)) {
      cur = cur->next = new_token(TK_NUM, p, p);
      char* q = p;
      /* strtol(p, &p, 10) means converting the string 
       * p to a 'long int' according to the given base 10 */
      cur->val = strtol(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    /* identifiers or keywords */
    if (is_ident1(*p)) {
      char* start = p;
      do {
        p++;
      } while (is_ident2(*p));
      cur = cur->next = new_token(TK_IDENT, start, p);
      continue;
    }

    /* punctuators */
    int punct_len = read_punct(p);
    if (punct_len) {
      cur = cur->next = new_token(TK_PUNCT, p, p + punct_len);
      p += punct_len;
      continue;
    }
    /* if nothing is right, then show invalid tokens */
    error_at(p, "invalid token!");
  }
  /* set the end-of-file marker */
  cur = cur->next = new_token(TK_EOF, p, p);
  convert_keywords(head.next);
  return head.next;   /* here `next` is an element in `head` */
}

/* debug and show all the tokens */
void showTokens(Token* tk) {
  for (Token* head = tk; head != NULL; head = head->next) {
    printf("Token val: %d Token len: %d Token loc: %*.*s      Token type: %d\n", head->val, head->len, 8, head->len, head->loc, head->kind);
  }
}
