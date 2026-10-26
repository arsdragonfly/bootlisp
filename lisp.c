#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <stdint.h>

#if UINTPTR_MAX != 0xFFFFFFFFu
#error "Not 32-bit architecture"
#endif

#define ARENA_SIZE (1 << 20)
enum tag { TAG_PTR, TAG_NUMBER, TAG_REF, TAG_VECTOR };
enum builtin {
  BUILTIN_INVAL,
  BUILTIN_QUOTE,
  BUILTIN_CAR,
  BUILTIN_CDR,
  BUILTIN_CONS,
  BUILTIN_EQ,
  BUILTIN_PAIR,
  BUILTIN_IF,
  BUILTIN_ADD,
  BUILTIN_NEG,
  BUILTIN_CTX,
  BUILTIN_LAMBDA,
  BUILTIN_END,
};

typedef struct cell cell;
typedef union val val;

union val {
  cell *p;
  struct {
    unsigned tag : 2;
    unsigned m : 1;
    signed n : 29;
  } v;
};
struct cell {
  val car, cdr;
};

val inval = {.v = {.tag = TAG_REF, .m = 0, .n = BUILTIN_INVAL}};
int invalid(val v) { return v.v.tag == TAG_REF && v.v.n == BUILTIN_INVAL; }

val tagval(int tag, int n) { return (val){.v = {.tag = tag, .n = n}}; }

#define NONNULL(expr)                                                          \
  ({                                                                           \
    val nonnull_ = (val)(expr);                                                \
    if (nonnull_.v.tag != TAG_PTR || nonnull_.p == NULL)                       \
      return inval;                                                            \
    nonnull_;                                                                  \
  })
#define CAR(expr)                                                              \
  ({                                                                           \
    val pair_ = NONNULL(expr);                                                 \
    pair_.p->car;                                                              \
  })
#define CDR(expr)                                                              \
  ({                                                                           \
    val pair_ = NONNULL(expr);                                                 \
    pair_.p->cdr;                                                              \
  })
#define NUMBER(expr)                                                           \
  ({                                                                           \
    val number_ = (val)(expr);                                                 \
    if (number_.v.tag != TAG_NUMBER)                                           \
      return inval;                                                            \
    number_.v.n;                                                               \
  })
#define VALID(expr)                                                            \
  ({                                                                           \
    val value_ = (val)(expr);                                                  \
    if (invalid(value_))                                                       \
      return inval;                                                            \
    value_;                                                                    \
  })

struct arena {
  cell arena[ARENA_SIZE];
  cell *free;
} gbl = {0};

void init_arena() {
  memset(gbl.arena, 0, sizeof(cell) * ARENA_SIZE);
  gbl.free = gbl.arena;
  gbl.free->car = tagval(TAG_VECTOR, ARENA_SIZE - 1);
  gbl.free->cdr.p = NULL;
}

cell *cellvalloc(int size) {
  if (size % 2 == 0) {
    size++;
  }
  for (cell *f = gbl.free, **pf = &gbl.free; f;
       pf = &(f->cdr.p), f = f->cdr.p) {
    if (size == f->car.v.n) {
      *pf = f->cdr.p;
      f->cdr = (val){.v = {.tag = TAG_VECTOR, .m = 1, .n = 0}};
      return f;
    } else if (size < f->car.v.n) {
      f->car.v.n = f->car.v.n - size - 1;
      f = f + 1 + f->car.v.n;
      f->car = tagval(TAG_VECTOR, size);
      f->cdr = (val){.v = {.tag = TAG_VECTOR, .m = 1, .n = 0}};
      return f;
    }
  }
  fprintf(stderr, "error: out of memory\n");
  exit(1);
}

cell *toptr(val v) { return (cell *)((uintptr_t)(v.p) & ~0b111); }
cell *isptr(val v) { return v.v.tag == TAG_PTR ? toptr(v) : NULL; }
#define GCMARK(x) (toptr((val)x)->car.v.m)
#define GCFLAG(x) (toptr((val)x)->cdr.v.m)
void gcmarkv(val v) { toptr(v)->car.v.m = 1; }
#define GCMARKLEAF(x, cont, ...)                                               \
  if (!isptr((x)) || GCMARK((x))) {                                            \
    if ((x).v.tag == TAG_VECTOR) {                                             \
      gcmarkv((x));                                                            \
    }                                                                          \
    cont;                                                                      \
  } else {                                                                     \
    __VA_ARGS__                                                                \
  }
void rotate4(val *a, val *b, val *c, val *d) {
  *a = (val){.v = {.tag = b->v.tag, .m = a->v.m, .n = b->v.n}};
  *b = (val){.v = {.tag = c->v.tag, .m = b->v.m, .n = c->v.n}};
  *c = (val){.v = {.tag = d->v.tag, .m = c->v.m, .n = d->v.n}};
  *d = (val){.v = {.tag = a->v.tag, .m = d->v.m, .n = a->v.n}};
}

void gcmark(val root) {
  root.v.m = 0;
  GCMARKLEAF(root, { return; });
  val prev = {}, tmp = {};
  while (root.p) {
    GCMARKLEAF(root.p->car, {
      GCMARK(root) = 1;
      GCMARKLEAF(root.p->cdr, { GCFLAG(root) = 1; })
    })
    if (GCFLAG(root)) {
      GCFLAG(root) = 0;
      while (prev.p) {
        if (GCFLAG(prev)) {
          GCFLAG(prev) = 0;
          rotate4(&tmp, &prev.p->cdr, &root, &prev);
        } else {
          GCMARKLEAF(prev.p->cdr, rotate4(&tmp, &prev.p->car, &root, &prev);
                     , { break; })
        }
      }
      if (!prev.p) {
        return;
      }
      GCFLAG(prev) = 1;
      rotate4(&tmp, &prev.p->car, &root, &prev.p->cdr);
    } else if (GCMARK(root)) {
      GCFLAG(root) = 1;
      rotate4(&tmp, &root.p->cdr, &prev, &root);
    } else {
      GCMARK(root) = 1;
      rotate4(&tmp, &root.p->car, &prev, &root);
    }
  }
}

int gccell(cell *c) {
  if (c->car.v.m == 1) {
    {
      c->car.v.m = 0;
      if (c->cdr.v.m != 1) {
        return 0;
      }
      return -(c->car.v.n);
    }
  }
  if (c->cdr.v.m != 1) {
    return 1;
  }
  return 1 + c->car.v.n;
}

void gcsweep() {
  cell *p = gbl.arena;
  cell **f = &gbl.free;
  cell *b = NULL;
  int result;
  while (*f && *f == p) {
    p += (*f)->car.v.n + 1;
    b = *f;
    f = &((*f)->cdr.p);
  }
  while (p < gbl.arena + ARENA_SIZE) {
    result = gccell(p);
    if (result <= 0) {
      p += 1 - result;
      while (*f && *f == p) {
        p += (*f)->car.v.n + 1;
        b = *f;
        f = &((*f)->cdr.p);
      }
    } else {
      if (b && p == b + 1 + b->car.v.n) {
        b->car.v.n += result;
        p += result;
      } else {
        p->car = (val){.v = {.tag = TAG_VECTOR, .n = result - 1}};
        p->cdr.p = *f;
        b = *f = p;
        f = &((*f)->cdr.p);
        p += result;
      }
      while (*f && *f == p) {
        b->car.v.n += (*f)->car.v.n + 1;
        p += (*f)->car.v.n + 1;
        *f = (*f)->cdr.p;
      }
    }
  }
}

void gc(val *roots, int nroots) {
  for (int i = 0; i < nroots; i++) {
    gcmark(roots[i]);
  }
  gcsweep();
}

char *numerals = "0123456789";

cell *cellalloc() {
  cell *f = gbl.free, **b = &gbl.free;
  if (f) {
    if (f->car.v.n == 0) {
      *b = f->cdr.p;
      memset(f, 0, sizeof(cell));
      return f;
    } else {
      memset(f + f->car.v.n, 0, sizeof(cell));
      f->car.v.n -= 1;
      return f + 1 + f->car.v.n;
    }
  }
  fprintf(stderr, "error: out of memory\n");
  exit(1);
}

cell *cellnew(val car, val cdr) {
  cell *p = cellalloc();
  p->car = car;
  p->cdr = cdr;
  return p;
}

typedef struct parser parser;

struct parser {
  int32_t c;
};

int advance_parser(parser *par) { return par->c = getchar(); }

char *parse_symbol(parser *par, char *cs, int consume) {
  char *pc = cs;
  for (; pc && *pc != '\0' && par->c != *pc; pc++) {
  }
  if (*pc && consume) {
    advance_parser(par);
  }
  return *pc ? pc : NULL;
}

int32_t parse_multi(parser *par, char *cs) {
  int32_t sum = 0, len = strlen(cs);
  char *pc;
  while ((pc = parse_symbol(par, cs, 1))) {
    sum = sum * len + (pc - cs);
  }
  return sum;
}

int32_t parse_natural(parser *par) { return parse_multi(par, numerals); }

void parse_rest_of_line(parser *par) {
  while (par->c != EOF && par->c != '\n') {
    advance_parser(par);
  }
}

val parse_sexp(parser *par) {
  val sexp = {.p = NULL}, sexp2;
  val *p_sexp = &sexp;
  parse_multi(par, " ");
  if (parse_symbol(par, numerals, 0)) {
    return tagval(TAG_NUMBER, parse_natural(par));
  } else if (parse_symbol(par, "-", 1)) {
    return tagval(TAG_NUMBER, -parse_natural(par));
  } else if (parse_symbol(par, "%", 1)) {
    if (par->c == '0') {
      return inval;
    }
    return tagval(TAG_REF, -parse_natural(par));
  } else if (parse_symbol(par, "@", 1)) {
    return tagval(TAG_REF, parse_natural(par));
  } else if (parse_symbol(par, "/", 1)) {
    return tagval(TAG_REF, BUILTIN_LAMBDA);
  } else if (parse_symbol(par, "$", 1)) {
    return tagval(TAG_REF, BUILTIN_CTX);
  } else if (parse_symbol(par, "(", 1)) {
    while (1) {
      while (parse_symbol(par, " \n", 0)) {
        parse_multi(par, " ");
        if (parse_symbol(par, "\n", 0)) {
          printf("... ");
          advance_parser(par);
        }
      }
      sexp2 = parse_sexp(par);
      if (invalid(sexp2)) {
        if (!parse_symbol(par, ")", 1)) {
          return inval;
        }
        return sexp;
      }
      p_sexp->p = cellnew(sexp2, (val)(cell *)NULL);
      p_sexp = &(p_sexp->p->cdr);
    }
  }
  return inval;
}

void print_atom(val sexp) {
  if (sexp.v.tag == TAG_REF && sexp.v.n == BUILTIN_LAMBDA) {
    printf("/");
  } else if (invalid(sexp)) {
    printf("#INVAL");
  } else if (sexp.v.tag == TAG_REF && sexp.v.n > 0) {
    printf("@%d", sexp.v.n);
  } else if (sexp.v.tag == TAG_REF && sexp.v.n < 0) {
    printf("%%%d", (-sexp.v.n));
  } else if (sexp.v.tag == TAG_VECTOR) {
    printf("(#vec %x %d %d %.*s)", sexp.v.n, toptr(sexp)->car.v.n,
           toptr(sexp)->cdr.v.n, toptr(sexp)->cdr.v.n,
           (char *)(toptr(sexp) + 1));
  } else {
    printf("%d", sexp.v.n);
  }
}

void print_sexp(val sexp) {
  if (sexp.v.tag == TAG_PTR) {
    printf("(");
    cell *p = sexp.p;
    while (p) {
      print_sexp(p->car);
      if (p->cdr.p != NULL) {
        printf(" ");
      }
      if (p->cdr.v.tag == TAG_PTR) {
        p = p->cdr.p;
      } else {
        printf(". ");
        print_atom(p->cdr);
        break;
      }
    }
    printf(")");
  } else {
    print_atom(sexp);
  }
}

val eval_sexp(val sexp, val *p_ctx, cell *env);
val evcar(val sexp, val *p_ctx, cell *env, val *outcdr);
val apply_sexp(val car, val cdr, val *p_ctx, cell *env);

val eval_sexp(val sexp, val *p_ctx, cell *env) {
  if (sexp.v.tag == TAG_NUMBER || (sexp.v.tag == TAG_REF && sexp.v.n >= 0)) {
    return sexp;
  } else if (sexp.v.tag == TAG_REF && sexp.v.n < 0) {
    cell *p = env;
    int32_t step = sexp.v.n + 1;
    while (p && step) {
      p = CDR(p).p;
      step++;
    }
    return CAR(p);
  } else if (sexp.v.tag == TAG_PTR) {
    if (sexp.p == NULL) {
      return sexp;
    }
    val car = eval_sexp(sexp.p->car, p_ctx, env);
    return apply_sexp(car, sexp.p->cdr, p_ctx, env);
  }
  return inval;
}

val evcar(val sexp, val *p_ctx, cell *env, val *outcdr) {
  val result = eval_sexp(CAR(sexp), p_ctx, env);
  *outcdr = CDR(sexp);
  return result;
}

val apply_sexp(val car, val cdr, val *p_ctx, cell *env) {
  if (car.v.tag == TAG_REF && car.v.n >= 0) {
    val cadr, caddr;
    if (car.v.n >= BUILTIN_END) {
      return inval;
    }
    if (car.v.n == BUILTIN_CTX && cdr.p == NULL) {
      return *p_ctx;
    }
    cadr = CAR(cdr);
    if (car.v.n == BUILTIN_QUOTE) {
      return cadr;
    }
    if (car.v.n == BUILTIN_LAMBDA) {
      return (val)cellnew((val)env, cadr);
    }
    cadr = VALID(evcar(cdr, p_ctx, env, &cdr));
    switch (car.v.n) {
    case BUILTIN_CTX:
      *p_ctx = cadr;
      return *p_ctx;
    case BUILTIN_NEG:
      return tagval(TAG_NUMBER, -NUMBER(cadr));
    case BUILTIN_PAIR:
      return cadr.v.tag == TAG_PTR ? cadr : (val)(cell *)NULL;
    case BUILTIN_CAR:
      return CAR(cadr);
    case BUILTIN_CDR:
      return CDR(cadr);
    case BUILTIN_IF:
      return cadr.p != NULL ? eval_sexp(CAR(cdr), p_ctx, env)
                            : eval_sexp(CAR(CDR(cdr)), p_ctx, env);
    default:
      caddr = VALID(evcar(cdr, p_ctx, env, &cdr));
      switch (car.v.n) {
      case BUILTIN_CONS:
        return (val)cellnew(cadr, caddr);
      case BUILTIN_EQ:
        return cadr.p == caddr.p ? tagval(TAG_NUMBER, 1) : (val)(cell *)NULL;
      case BUILTIN_ADD:
        if (cadr.v.tag == TAG_NUMBER) {
          return tagval(TAG_NUMBER, cadr.v.n + NUMBER(caddr));
        } else if (cadr.v.tag == TAG_PTR) {
          return (val)(cadr.p + NUMBER(caddr));
        } else {
          return inval;
        }
      }
    }
  } else if (car.v.tag == TAG_PTR) {
    val clenv = CAR(car), body = CDR(car), cadr = CAR(cdr);
    val newenv = (val)cellnew(eval_sexp(cadr, p_ctx, env), clenv);
    return eval_sexp(body, p_ctx, newenv.p);
  }
  return inval;
}

void read_line(parser *par, val *p_ctx) {
  val sexp;
  while (1) {
    sexp = parse_sexp(par);
    if (invalid(sexp)) {
      if (par->c != '\n' && par->c != EOF) {
        fprintf(stderr, "failed to parse sexp at %c\n", par->c);
        parse_rest_of_line(par);
      }
      return;
    }
    print_sexp(eval_sexp(sexp, p_ctx, NULL));
    /* test */
    if (p_ctx && isptr(*p_ctx)) {
      gc(p_ctx, 1);
    }
    printf("\n");
  }
}

int main() {
  parser par = {.c = '\n'};
  val ctx = {.p = NULL};
  init_arena();
  while (par.c != EOF) {
    printf("lisp>");
    advance_parser(&par);
    read_line(&par, &ctx);
  }
  return 0;
}
