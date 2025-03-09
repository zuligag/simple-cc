#include <stdio.h>
#include <stdint.h>
typedef unsigned long size_t;
typedef long ssize_t;

// Simplified posix standard
//#define NULL 0
int open(const char *path, int oflag);
ssize_t read(int fd, void* buf, size_t nb);
ssize_t write(int fd, const void *buf, size_t nb);
int close(int fd);

void free(void *p);
void * malloc(size_t cb);
void * realloc(void *p, size_t cb);

void * memset(void *p, int v, size_t c);
void * memcpy(void *dst, const void*src, size_t cb);
int memcmp(const void *a, const void *b, size_t cb);

void exit(int status);

#define MAX_FILE (128 * 1024)

enum {
  Eof = 0,
  Id = 'a', Num = '0',
  Minus = '-', Dec = 'd', Plus = '+', Inc = 'i',
  Shl = 'S', Shr = 's', Lt = '<', Gt = '>',
  Eq = '=', Asgn = '@' , Neq = 'N', Leq = 'l', Geq = 'g',
  And = '&', Or = '|', Not = '!', Lor = 'O', Land = 'A', 
  Mul = '*', Div = '/',
  LBrak = '[', RBrak = ']',
  LParen = '(', RParen = ')',
  LBrac = '{', RBrac = '}',
  Dot = '.', Comma = ',',
  Colon = ':', Semi = ';',
  Char = '\'', Str = '"'
};

typedef struct sym_s {
  int hash;
  int len;
  char s[0];
} sym_t;

char *p, *lp,
  *sym,
  *d, *dp;
long n;
char tk;
int32_t *e;
sym_t* id;
long ln = 0;


int puts(const char * sz) {
  int i=0;
  const char * nl = "\n";
  while(sz[i]) i++;
  write(1, sz, i);
  write(1, nl, 1);
  return 0;
}

void next() {
  id = (sym_t*)sym;
  while (!!(tk = *p)) {
    while (lp != p) {
      if (*lp == '\n') ++ln;
      ++lp;
    }
    // lp == p
    ++p;
    if ((tk >= 'a' && tk <= 'z') ||
	(tk >= 'A' && tk <= 'Z') ||
	(tk == '_')) {
      int hash = (int)tk;
      while (!!(tk = *p) &&
	     ((tk >= 'a' && tk <= 'z') ||
	      (tk >= 'A' && tk <= 'Z') ||	
	      (tk >= '0' && tk <= '9') ||	
	      (tk == '_'))) {
	hash = (hash << 8) + hash * 13 + tk;
	++p;
      }
      tk = Id;
      ssize_t len = p - lp;
      id = (sym_t*)sym;
      while (id->len) {
	if ((id->hash == hash && id->len == len) && !memcmp(lp, id->s, len)) {
	  return;
	}
	id = (sym_t*)(((char*)id)+sizeof(sym_t)+id->len+1);
      }
      id->hash = hash;
      id->len = len;
      memcpy(id->s, lp, len);
      return;
    } else if ((tk >= '0' && tk <= '9') || (tk == '-' && *p >= '0' && *p <= '9')) {
      int neg = (tk == '-');
      if (neg) {
	tk = *p;
	++p;
      }
      n = -(tk - '0');
      while (!!(tk = *p) && (tk >= '0' && tk <= '9')) {
	// TODO: how to detect overflow?
	n = n * 10 - (tk - '0');
	++p;
      }
      if (neg) {
	n = -n;
      }
      tk = Num;
      return;
    } else if (tk == '\'') {
      tk = Char;
      if (*p && p[0] != '\\' && p[1] == '\'') {
	n = *p;
	++p;
	return;
      } else if (p[0] == '\\' && p[1] && p[2] == '\'') {
	switch (p[1]) {
	case '\\': n = '\\'; break;
	case '\'': n = '\''; break;
	case 'n':  n = '\n'; break;
	case 'r':  n = '\r'; break;
	case 't':  n = '\t'; break;
	default:   n = p[1]; break; // TODO: fixme
	}
	p += 2;
	return;
      } else {
	printf("bad character literal on line %ld\n", ln);
	break;
      }
    } else if (tk == '"') {
      dp = d;
      while (*p && *p != '"') {
	if (*p == '\\') {
	  ++p;
	  switch (*p) {
	  case '\\': *d = '\\'; break;
	  case '\"': *d = '\"'; break;
	  case 'n':  *d = '\n'; break;
	  case 'r':  *d = '\r'; break;
	  case 't':  *d = '\t'; break;
	  default:   *d = *p; break; // TODO: improve?
	  }
	} else {
	  *d = *p;
	}
	++d;
	++p;
      }
      *d = 0;
      d++;
      p++;
      tk = Str;
      return;
    } else if (tk == '#') {
      while (*p && *p != '\n') ++p;
    } else {
      switch (tk) {
      case ' ':
      case '\t':
      case '\n':
	break; // whitespace
      case '-':
	if (*p == '-') { ++p; tk = Dec; } else { tk = Minus; }
	return;
      case '+':
	if (*p == '+') { ++p; tk = Inc; } else { tk = Plus; }
	return;
      case '&':
	if (*p == '&') { ++p; tk = And; } else { tk = Land; }
	return;
      case '|':
	if (*p == '&') { ++p; tk = Or; } else { tk = Lor; }
	return;
      case '=':
	if (*p == '=') { ++p; tk = Eq; } else { tk = Asgn; }
	return;
      case '!':
	if (*p == '=') { ++p; tk = Neq; } else { tk = Not; }
	return;
      case '<':
	if (*p == '<') { ++p; tk = Shl; }
	else if (*p == '=') { tk = Leq; }
	else { tk = Lt; }
	return;
      case '>':
	if (*p == '>') { ++p; tk = Shr; }
	else if (*p == '=') { tk = Geq; }
	else { tk = Gt; }
	return;

      case '/':
	if (*p == '/') { while (*p && *p != '\n') ++p; break; }
	tk = Div;
	return;

      case '*':
      case '.':
      case ',':
      case '(':
      case ')':
      case '{':
      case '}':
      case '[':
      case ']':
      case ';':
      case ':':
	return;
      default:
	printf("unhandled char '%c'\n", tk);
	exit(1);
      }
    }
  }
  // EOF
}


int main(int argc, const char** argv) {
  ssize_t cb;
  int fd;
  
  if (!(p = malloc(MAX_FILE))) { puts("could not malloc src-code memory."); return 1; }
  if (!(sym = malloc(MAX_FILE))) { puts("could not malloc sym memory."); return 1; }
  if (!(d = malloc(MAX_FILE))) { puts("could not malloc data memory."); return 1; }

  lp = memset(p, 0, MAX_FILE);
  memset(sym, 0, MAX_FILE);
  memset(d, 0, MAX_FILE);

  if (0 > (fd = open(argv[1], 0))) { printf("Unable to open '%s'\n", argv[1]); return 1; }
  cb = read(fd, p, MAX_FILE);
  close(fd);

  char * sp = p;
  for (fd=0; fd<200; fd++) {
    next();
    printf("[%d:%d#%d] %c %d'%s'\n", (int)(lp - sp), (int)(p - lp), (int)ln, (char)tk, (int)((char*)id - sym), (tk==Id) ? id->s : (tk==Str ? dp : NULL));
    if (tk == Eof) break;
  }
}
