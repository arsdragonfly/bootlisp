#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#if UINTPTR_MAX != 0xFFFFFFFFu
#error "Not 32-bit architecture"
#endif

#define ARENA_SIZE 1 << 20
enum tag {
  TAG_PTR = 0,
  TAG_NUMBER,
  TAG_REF,
  TAG_BUILTIN,
  TAG_INVAL = 7
};
enum builtin {
  BUILTIN_LAMBDA = 0,
  BUILTIN_T,
  BUILTIN_QUOTE,
  BUILTIN_CAR,
  BUILTIN_CDR,
  BUILTIN_CONS,
  BUILTIN_EQ,
  BUILTIN_PAIR,
  BUILTIN_IF,
  BUILTIN_ADD,
  BUILTIN_SUB,
  BUILTIN_CTX,
};

typedef struct cell cell;
typedef union val val;

union val {
  cell *p;
  struct {
    unsigned tag : 3;
    signed n : 29;
  } v;
};
struct cell {
  val car, cdr;
};

val inval = {.v = {TAG_INVAL, 0}};

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
    if (value_.v.tag == TAG_INVAL)                                             \
      return inval;                                                            \
    value_;                                                                    \
  })

struct arena {
  cell arena[ARENA_SIZE];
  size_t idx;
} gbl = {0};

char *numerals = "0123456789";

cell *cellalloc() {
  if (gbl.idx < ARENA_SIZE) {
    gbl.idx += 1;
    return gbl.arena + gbl.idx;
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
  while (pc && *pc != '\0' && par->c != *pc) {
    pc++;
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

void parse_rest_of_line(parser *par) {
  while (par->c != EOF && par->c != '\n') {
    advance_parser(par);
  }
}

val parse_sexp(parser *par) {
  val sexp = {.p = NULL}, sexp2;
  char prefix[] = "%@";
  char *charidx;
  int tags[] = {TAG_REF, TAG_BUILTIN};
  val *p_sexp = &sexp;
  parse_multi(par, " ");
  if (parse_symbol(par, numerals, 0)) {
    return tagval(TAG_NUMBER, parse_multi(par, numerals));
  } else if ((charidx = parse_symbol(par, prefix, 1))) {
    return tagval(tags[charidx - prefix], parse_multi(par, numerals));
  } else if (parse_symbol(par, "/", 1)) {
    return tagval(TAG_BUILTIN, BUILTIN_LAMBDA);
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
      if (sexp2.v.tag == TAG_INVAL) {
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
  if (sexp.v.tag == TAG_BUILTIN && sexp.v.n == BUILTIN_LAMBDA) {
    printf("/");
  } else if (sexp.v.tag == TAG_INVAL) {
    printf("#INVAL");
  } else {
    char *prefixes = "  %$ @";
    if (sexp.v.tag != TAG_NUMBER) {
      printf("%c", prefixes[sexp.v.tag]);
    }
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
      }
    }
    printf(")");
  } else {
    print_atom(sexp);
  }
}

val eval_sexp(val sexp, val *p_ctx, cell *env);
val apply_sexp(val car, val cdr, val *p_ctx, cell *env);

val eval_sexp(val sexp, val *p_ctx, cell *env) {
  if (sexp.v.tag == TAG_NUMBER || sexp.v.tag == TAG_BUILTIN ||
      sexp.v.tag == TAG_INVAL) {
    return sexp;
  } else if (sexp.v.tag == TAG_REF) {
    cell *p = env;
    size_t step = sexp.v.n;
    while (p && step) {
      p = CDR(p).p;
      step--;
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

val apply_sexp(val car, val cdr, val *p_ctx, cell *env) {
  if (car.v.tag != TAG_PTR && car.v.tag != TAG_BUILTIN) {
    return inval;
  } else if (car.v.tag == TAG_BUILTIN) {
    if (car.v.n == BUILTIN_T) {
      return car;
    } else if (car.v.n == BUILTIN_CTX && cdr.p == NULL) {
      return *p_ctx;
    }
    val cadr = CAR(cdr);
    if (car.v.n == BUILTIN_QUOTE) {
      return cadr;
    } else if (car.v.n == BUILTIN_LAMBDA) {
      return (val)cellnew((val)env, cadr);
    } else {
      cadr = VALID(eval_sexp(cadr, p_ctx, env));
      if (car.v.n == BUILTIN_CTX) {
        *p_ctx = cadr;
        return *p_ctx;
      } else if (car.v.n == BUILTIN_CAR || car.v.n == BUILTIN_CDR ||
                 car.v.n == BUILTIN_PAIR) {
        if (cadr.v.tag != TAG_PTR || cadr.p == NULL) {
          if (car.v.n == BUILTIN_PAIR) {
            return (val)(cell *)NULL;
          }
          return inval;
        } else {
          if (car.v.n == BUILTIN_CAR) {
            return cadr.p->car;
          } else if (car.v.n == BUILTIN_CDR) {
            return cadr.p->cdr;
          } else {
            return cadr;
          }
        }
      } else if (car.v.n == BUILTIN_CONS || car.v.n == BUILTIN_EQ ||
                 car.v.n == BUILTIN_ADD || car.v.n == BUILTIN_SUB) {
        val caddr = CAR(CDR(cdr));
        caddr = VALID(eval_sexp(caddr, p_ctx, env));
        if (car.v.n == BUILTIN_EQ) {
          val result;
          if (cadr.v.tag == caddr.v.tag &&
              (cadr.v.tag == TAG_PTR ? cadr.p == caddr.p
                                     : cadr.v.n == caddr.v.n)) {
            result.v.tag = TAG_BUILTIN;
            result.v.n = BUILTIN_T;
          } else {
            result.p = NULL;
          }
          return result;
        } else if (car.v.n == BUILTIN_ADD || car.v.n == BUILTIN_SUB) {
          return tagval(TAG_NUMBER, (car.v.n == BUILTIN_ADD)
                                        ? (NUMBER(cadr) + NUMBER(caddr))
                                        : (NUMBER(cadr) - NUMBER(caddr)));
        } else {
          return (val)cellnew(cadr, caddr);
        }
      } else if (car.v.n == BUILTIN_IF) {
        if (cadr.p != NULL) {
          return eval_sexp(CAR(CDR(cdr)), p_ctx, env);
        } else {
          return eval_sexp(CAR(CDR(CDR(cdr))), p_ctx, env);
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
    if (sexp.v.tag == TAG_INVAL) {
      if (par->c != '\n' && par->c != EOF) {
        fprintf(stderr, "failed to parse sexp at %d\n", par->c);
        parse_rest_of_line(par);
      }
      return;
    }
    print_sexp(eval_sexp(sexp, p_ctx, NULL));
    printf("\n");
  }
}

int main() {
  parser par;
  val ctx = {.p = NULL};
  while (par.c != EOF) {
    printf("lisp>");
    advance_parser(&par);
    read_line(&par, &ctx);
  }
  return 0;
}
