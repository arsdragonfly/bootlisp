#include "stdio.h"
#include "stdint.h"
#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "stdint.h"

#if UINTPTR_MAX != 0xFFFFFFFFu
	#error "Not 32-bit architecture"
#endif

#define ARENA_SIZE 1 << 20
enum tag {
	TAG_PTR = 0,
	TAG_NUMBER,
	TAG_REF,
	TAG_ENV,
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
};

typedef struct cell cell;
typedef union val val;
typedef struct tagval tagval;

struct tagval {
	unsigned tag : 3;
	signed   n   : 29;
};
union val {
	cell* p;
	tagval v;
};
struct cell {
	val car, cdr;
};

val inval = {.v = {TAG_INVAL, 0}};

#define NONNULL(expr) ({val nonnull_ = (val)(expr); if (nonnull_.v.tag != TAG_PTR || nonnull_.p == NULL) return inval; nonnull_;})
#define CAR(expr) ({val pair_ = NONNULL(expr); pair_.p->car;})
#define CDR(expr) ({val pair_ = NONNULL(expr); pair_.p->cdr;})
#define NUMBER(expr) ({ val number_ = (val)(expr); if (number_.v.tag != TAG_NUMBER) return inval; number_.v.n;})

struct arena {
	cell arena[ARENA_SIZE];
	size_t idx;
} gbl = {0};

char* numerals = "0123456789";

cell* cellalloc() {
	if (gbl.idx < ARENA_SIZE) {
		gbl.idx += 1;
		return gbl.arena + gbl.idx;
	}
	fprintf(stderr, "error: out of memory\n");
	exit(1);
}

cell* cellnew(val car, val cdr) {
	cell* p = cellalloc();
	p->car = car;
	p->cdr = cdr;
	return p;
}

typedef struct parser parser;

struct parser { int32_t c; };

int advance_parser(parser* par) {
	return par->c = getchar();
}

char* parse_symbol(parser* par, char* cs, int consume) {
	char* pc = cs;
	while (pc && *pc != '\0' && par->c != *pc) {
		pc++;
	}
	if (*pc && consume) {
		advance_parser(par);
	}
	return *pc ? pc : NULL;
}

int32_t parse_multi(parser* par, char* cs) {
	int32_t sum = 0, len = strlen(cs);
	char* pc;
	while ((pc = parse_symbol(par, cs, 1))) {
		sum = sum * len + (pc - cs);
	}
	return sum;
}

void parse_rest_of_line(parser* par) {
	while (par->c != EOF && par->c != '\n') {
		advance_parser(par);
	}
}

val parse_sexp(parser* par) {
	val sexp, sexp2;
	char prefix[3] = "%$@";
	char* charidx;
	int tags[3] = {TAG_REF, TAG_ENV, TAG_BUILTIN};
	val *p_sexp = &sexp;
	sexp.v.tag = TAG_INVAL;
	sexp.v.n   = 0;
	parse_multi(par, " ");
	if (parse_symbol(par, numerals, 0)) {
		sexp.v.tag = TAG_NUMBER;
		sexp.v.n   = parse_multi(par, numerals);
		return sexp;
	} else if ((charidx = parse_symbol(par, prefix, 1))) {
		sexp.v.tag = tags[charidx - prefix];
		sexp.v.n   = parse_multi(par, numerals);
		return sexp;
	} else if (parse_symbol(par, "/", 1)) {
		sexp.v.tag = TAG_BUILTIN;
		sexp.v.n   = BUILTIN_LAMBDA;
		return sexp;
	} else if (parse_symbol(par, "(", 1)) {
		sexp.p = NULL;
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
					goto fail;
				}
				return sexp;
			}
			p_sexp->p = cellnew(sexp2, (val)(cell *)NULL);
			p_sexp           = &(p_sexp->p->cdr);
		}
	}
fail:
	sexp.v.tag = TAG_INVAL;
	sexp.v.n   = par->c;
	return sexp;
}

void print_atom(val sexp)
{
	if (sexp.v.tag == TAG_BUILTIN && sexp.v.n == BUILTIN_LAMBDA) {
		printf("/");
	} else if (sexp.v.tag == TAG_INVAL) {
		printf("#INVAL");
	} else {
		char * prefixes = "  %$ @";
		if (sexp.v.tag != TAG_NUMBER) {
			printf("%c", prefixes[sexp.v.tag]);
		}
		printf("%d", sexp.v.n);
	}
}

void print_sexp(val sexp)
{
	if (sexp.v.tag == TAG_PTR) {
		printf("(");
		cell* p = sexp.p;
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

val eval_sexp(val sexp, cell** p_ctx, cell* env);
val apply_sexp(val car, val cdr, cell** p_ctx, cell* env);

val eval_sexp(val sexp, cell** p_ctx, cell* env)
{
	if (sexp.v.tag == TAG_NUMBER ||
			sexp.v.tag == TAG_BUILTIN ||
			sexp.v.tag == TAG_INVAL) {
		return sexp;
	} else if (sexp.v.tag == TAG_ENV) {
		cell* p = *p_ctx;
		while (p) {
			if (NUMBER(CAR(CAR(p))) == sexp.v.n) {
				return CDR(CAR(p));
			}
			p = CDR(p).p;
		}
		return inval;
	} else if (sexp.v.tag == TAG_REF) {
		cell* p = env;
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

val apply_sexp(val car, val cdr, cell** p_ctx, cell* env) {
	if (car.v.tag != TAG_PTR && car.v.tag != TAG_BUILTIN) {
		goto fail;
	} else if (car.v.tag == TAG_BUILTIN) {
		if (car.v.n == BUILTIN_T) {
			return car;
		}
		if (cdr.v.tag != TAG_PTR || cdr.p == NULL) {
			goto fail;
		}
		val cadr = cdr.p->car;
		if (car.v.n == BUILTIN_QUOTE) {
			return cadr;
		} else if (car.v.n == BUILTIN_LAMBDA) {
			return (val)cellnew((val)env, cadr);
		} else {
			cadr = eval_sexp(cadr, p_ctx, env);
			if (cadr.v.tag == TAG_INVAL) {
				goto fail;
			}
			if (car.v.n == BUILTIN_CAR ||
					car.v.n == BUILTIN_CDR ||
					car.v.n == BUILTIN_PAIR) {
				if (cadr.v.tag != TAG_PTR || cadr.p == NULL) {
					if (car.v.n == BUILTIN_PAIR) {
						return (val)(cell *)NULL;
					}
					goto fail;
				} else {
					if (car.v.n == BUILTIN_CAR) {
						return cadr.p->car;
					} else if (car.v.n == BUILTIN_CDR) {
						return cadr.p->cdr;
					} else {
						return cadr;
					}
				}
			} else if (car.v.n == BUILTIN_CONS ||
					car.v.n == BUILTIN_EQ ||
					car.v.n == BUILTIN_ADD ||
					car.v.n == BUILTIN_SUB) {
				val cddr = cdr.p->cdr;
				if (cddr.v.tag != TAG_PTR || cddr.p == NULL) {
					goto fail;
				} else {
					val caddr = cddr.p->car;
					caddr = eval_sexp(caddr, p_ctx, env);
					if (caddr.v.tag == TAG_INVAL) {
						goto fail;
					}
					if (car.v.n == BUILTIN_EQ) {
						val result;
						if (cadr.v.tag == caddr.v.tag && (
									cadr.v.tag == TAG_PTR ?
									cadr.p == caddr.p : 
									cadr.v.n == caddr.v.n)) {
							result.v.tag = TAG_BUILTIN;
							result.v.n   = BUILTIN_T;
						} else {
							result.p = NULL;
						}
						return result;
					} else if (car.v.n == BUILTIN_ADD || car.v.n == BUILTIN_SUB) {
						if (cadr.v.tag != TAG_NUMBER || caddr.v.tag != TAG_NUMBER) {
							goto fail;
						}
						val result;
						result.v.tag = TAG_NUMBER;
						result.v.n = car.v.n == BUILTIN_ADD ?
							(cadr.v.n + caddr.v.n) :
							(cadr.v.n - caddr.v.n);
						return result;
					} else {
						return (val)cellnew(cadr, caddr);
					}
				}
			} else if (car.v.n == BUILTIN_IF) {
				val cddr = cdr.p->cdr;
				if (cddr.v.tag != TAG_PTR || cddr.p == NULL) {
					goto fail;
				} else {
					if (cadr.p != NULL) {
						val caddr = cddr.p->car;
						caddr = eval_sexp(caddr, p_ctx, env);
						return caddr;
					} else {
						val cdddr = cddr.p->cdr;
						if (cdddr.v.tag != TAG_PTR || cdddr.p == NULL) {
							goto fail;
						}
						val cadddr = cdddr.p->car;
						cadddr = eval_sexp(cadddr, p_ctx, env);
						return cadddr;
					}
				}
			} else {
				goto fail;
			}
		}
	} else if (car.v.tag == TAG_PTR) {
		if (car.p == NULL) {
			goto fail;
		}
		val clenv = car.p->car;
		val body = car.p->cdr;
		if (cdr.v.tag != TAG_PTR || cdr.p == NULL) {
			goto fail;
		}
		val cadr = cdr.p->car;
		cadr = eval_sexp(cadr, p_ctx, env);
		val newenv = (val)cellnew(cadr, clenv);
		return eval_sexp(body, p_ctx, newenv.p);
	}
fail:
	val result;
	result.v.tag = TAG_INVAL;
	return result;
}

void read_line(parser* par, cell** p_ctx) {
	val sexp;
	while (1) {
		sexp = parse_sexp(par); 
		if (sexp.v.tag == TAG_INVAL) {
			if (sexp.v.n != '\n' && sexp.v.n != EOF) {
				fprintf(stderr, "failed to parse sexp at %d\n", sexp.v.n);
				parse_rest_of_line(par);
			}
			return;
		}
		sexp = eval_sexp(sexp, p_ctx, NULL);
		int32_t ctx_index = (*p_ctx == NULL) ? 0 : (*p_ctx)->car.p->car.v.n + 1;
		val numtag = (val)(tagval){TAG_NUMBER, ctx_index};
		*p_ctx = cellnew((val)cellnew(numtag, sexp), (val)*p_ctx);
		printf("$%d = ", ctx_index);
		print_sexp(sexp);
		printf("\n");
	}
}

int main() {
	parser par;
	cell* ctx = NULL;
	while (par.c != EOF) {
		printf("lisp>");
		advance_parser(&par);
		read_line(&par, &ctx);
	}
	return 0;
}
